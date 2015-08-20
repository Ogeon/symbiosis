use std::io::{self, Write};
use std::collections::HashMap;
use std::fmt;

use tendril::StrTendril;

use string_cache::atom::Atom;
use html5ever::tokenizer::Doctype;

///Shortcut for the commonly used `try!(write!(...))`.
#[macro_export]
macro_rules! try_w {
    ($($args:tt)*) => (try!(write!($($args)*)))
}

///Code generators are used to generate template code in different languages
///and they have to implement this trait.
pub trait Codegen {
    type Error;

    ///Initiate the code writer.
    fn init_writer<'a, W: Write>(&self, w: &'a mut W) -> Writer<'a, W>;

    ///Generate code for a single template.
    fn build_template<W: Write>(&self, w: &mut Writer<W>, name: &str, params: &HashMap<StrTendril, ContentType>, tokens: &[Token]) -> Result<(), Self::Error>;

    ///Generate code for a module or a similar collection containing multiple templates.
    fn build_module<W, F>(&self, w: &mut Writer<W>, build_templates: F) -> Result<(), Self::Error> where
        W: Write,
        F: FnOnce(&mut Writer<W>) -> Result<(), Self::Error>
    {
        build_templates(w)
    }
}

///Tokens representing different parts of a template document.
pub enum Token {
    SetDoctype(Doctype),
    BeginTag(Atom),
    EndTag(bool),
    CloseTag(Atom),
    BeginAttribute(Atom, Content),
    AppendToAttribute(Content),
    EndAttribute,
    Text(Content),
    Scope(Scope),
    End,
}

///Representation of supported scope types.
pub enum Scope {
    ///Everything within an `if` scope will be hidden if the provided logic
    ///expression is false.
    If(Logic),
    ///Repeat the content within the scope for each element in a collection.
    ///Args: collection, element, optional key
    ForEach(StrTendril, StrTendril, Option<StrTendril>)
}

///Logic expressions.
pub enum Logic {
    ///a and b and ...
    And(Vec<Logic>),
    ///a or b or ...
    Or(Vec<Logic>),
    ///The logical complement of an expression.
    Not(Box<Logic>),
    ///A value from a template parameter.
    Value(StrTendril)
}

impl Logic {
    pub fn placeholders(&self) -> Vec<&StrTendril> {
        let mut res = vec![];
        self.placeholders_r(&mut res);
        res
    }

    fn placeholders_r<'a>(&'a self, res: &mut Vec<&'a StrTendril>) {
        match self {
            &Logic::And(ref conds) => for cond in conds {
                cond.placeholders_r(res);
            },
            &Logic::Or(ref conds) => for cond in conds {
                cond.placeholders_r(res);
            },
            &Logic::Not(ref cond) => cond.placeholders_r(res),
            &Logic::Value(ref name) => res.push(name)
        }
    }

    pub fn flattened(&self) -> Logic {
        match self {
            &Logic::And(ref conds) if conds.len() == 1 => conds.first().unwrap().flattened(),
            &Logic::And(ref conds) => Logic::And(conds.iter().map(|c| c.flattened()).collect()),
            &Logic::Or(ref conds) if conds.len() == 1 => conds.first().unwrap().flattened(),
            &Logic::Or(ref conds) => Logic::Or(conds.iter().map(|c| c.flattened()).collect()),
            &Logic::Not(ref cond) => match &**cond {
                &Logic::Not(ref cond) => cond.flattened(),
                other => Logic::Not(Box::new(other.flattened()))
            },
            &Logic::Value(ref name) => Logic::Value(name.clone())
        }
    }
}

///Types of text content.
pub enum Content {
    ///A plain string.
    String(StrTendril),

    ///Use content from a parameter in a placeholder.
    Placeholder(StrTendril)
}

///Types of template parameter content.
#[derive(Debug)]
pub enum ContentType {
    ///A plain (maybe optional) string.
    String(bool),
    ///A boolean value.
    Bool,
    ///An other (maybe optional) template.
    Template(bool),
    ///A (maybe optional) collection of hopefully inferred content, associated with a key.
    Collection(Option<Box<ContentType>>, bool)
}

impl ContentType {
    pub fn combine_with(&mut self, pref_ty: ContentType) -> Result<(), String> {
        match (self, pref_ty) {
            (this, ContentType::Bool) => this.set_optional(true),
            (this @ &mut ContentType::Bool, mut other) => {
                other.set_optional(true);
                *this = other;
            },
            (&mut ContentType::String(ref mut a_o), ContentType::String(ref mut b_o)) => *a_o = *a_o | *b_o,
            (&mut ContentType::Template(ref mut a_o), ContentType::Template(ref mut b_o)) => *a_o = *a_o | *b_o,
            (&mut ContentType::Collection(ref mut a, ref mut a_o), ContentType::Collection(ref mut b, ref mut b_o)) => {
                *a_o = *a_o | *b_o;
                match (a, b.take()) {
                    (&mut Some(ref mut a), ref mut b @ Some(_)) => try!(a.combine_with(*b.take().unwrap())),
                    (a @ &mut None, b @ Some(_)) => *a = b,
                    (&mut Some(_), None) |(&mut None, None)  => {}
                };
            },
            (a, b) => return Err(format!("content cannot be used as both {} and {}", a, b))
        }

        Ok(())
    }

    pub fn is_optional(&self) -> bool {
        match self {
            &ContentType::String(optional) => optional,
            &ContentType::Bool => false,
            &ContentType::Template(optional) => optional,
            &ContentType::Collection(_, optional) => optional,
        }
    }

    fn set_optional(&mut self, optional: bool) {
        match self {
            &mut ContentType::String(ref mut o) => *o = optional,
            &mut ContentType::Bool => {},
            &mut ContentType::Template(ref mut o) => *o = optional,
            &mut ContentType::Collection(_, ref mut o) => *o = optional,
        }
    }
}

impl fmt::Display for ContentType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.is_optional() {
            try!("optional ".fmt(f))
        }

        match self {
            &ContentType::String(_) => "string".fmt(f),
            &ContentType::Bool => "boolean value".fmt(f),
            &ContentType::Template(_) => "template".fmt(f),
            &ContentType::Collection(Some(ref a), _) => write!(f, "collection of {}", a),
            &ContentType::Collection(None, _) => "collection of something".fmt(f),
        }
    }
}

pub struct Writer<'a, W: Write + 'a> {
    indent_str: &'static str,
    base_indent: u8,
    indent: u8,
    writer: &'a mut W
}

impl<'a, W: Write> Writer<'a, W> {
    pub fn new(writer: &'a mut W, indent: &'static str) -> Writer<'a, W> {
        Writer {
            indent_str: indent,
            base_indent: 0,
            indent: 0,
            writer: writer
        }
    }

    pub fn begin_line<'w>(&'w mut self) -> Line<'w, W> {
        Line {
            writer: self.writer,
            indent: self.base_indent + self.indent,
            indent_str: self.indent_str,
            indent_written: false,
            end_written: false,
        }
    }

    pub fn indented_line<'w>(&'w mut self) -> Line<'w, W> {
        Line {
            writer: self.writer,
            indent: self.base_indent + self.indent + 1,
            indent_str: self.indent_str,
            indent_written: false,
            end_written: false
        }
    }

    pub fn write_fmt(&mut self, args: fmt::Arguments) -> io::Result<()> {
        let mut line = self.begin_line();
        try!(line.write_fmt(args));
        line.end()
    }

    pub fn indent(&mut self) {
        self.indent += 1;
    }

    pub fn unindent(&mut self) {
        if self.indent > 0 {
            self.indent -= 1;
        }
    }

    pub fn block<'w>(&'w mut self) -> Writer<'w, W> {
        Writer {
            indent_str: self.indent_str,
            base_indent: self.base_indent + self.indent + 1,
            indent: 0,
            writer: self.writer
        }
    }
}

pub struct Line<'a, W: Write + 'a> {
    writer: &'a mut W,
    indent: u8,
    indent_str: &'static str,
    indent_written: bool,
    end_written: bool
}

impl<'a, W: Write> Line<'a, W> {
    pub fn write_fmt(&mut self, args: fmt::Arguments) -> io::Result<()> {
        try!(self.write_indent());
        self.writer.write_fmt(args)
    }

    pub fn end(mut self) -> io::Result<()> {
        self.write_end()
    }

    #[inline]
    fn write_indent(&mut self) -> io::Result<()> {
        if !self.indent_written {
            for _ in 0..self.indent {
                try!(self.writer.write_all(self.indent_str.as_bytes()))
            }
            self.indent_written = true;
        }
        Ok(())
    }

    fn write_end(&mut self) -> io::Result<()> {
        if !self.end_written {
            self.end_written = true;
            self.writer.write_all(b"\n")
        } else {
            Ok(())
        }
    }
}

impl<'a, W: Write> Drop for Line<'a, W> {
    #[allow(unused_must_use)]
    fn drop(&mut self) {
        self.write_end();
    }
}