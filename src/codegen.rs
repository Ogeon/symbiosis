use std::io::{self, Write};
use std::fmt;
use std::ops::Deref;
use std::collections::hash_map::{HashMap, Values};
use std::hash::Hash;
use std::borrow::Borrow;

pub use symbiosis_tokenizer::codegen::{Name, Text, Path, Logic, Scope, Doctype};

use symbiosis_tokenizer::parser::Error;
use StrTendril;

pub enum Token {
    SetDoctype(Doctype),
    Comment(StrTendril),
    BeginTag(Name),
    EndTag(bool),
    CloseTag(Name),
    BeginAttribute(Name, Content),
    AppendToAttribute(Content),
    EndAttribute,
    Text(Content),
    Scope(Scope),
    End,
}

///Types of text content.
pub enum Content {
    ///A plain string.
    String(Text),

    ///Use content from a parameter in a placeholder.
    Placeholder(Path)
}

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

    ///Define all of the data structures, including the templates.
    fn build_structs<W: Write>(&self, w: &mut Writer<W>, structs: Structs) -> Result<(), Self::Error>;

    ///Generate rendering code for a single template.
    fn build_template<W: Write>(&self, w: &mut Writer<W>, name: &str, params: &HashMap<Name, Type>, structs: Structs, tokens: &[Token]) -> Result<(), Self::Error>;

    ///Generate code for a module or a similar collection containing multiple templates.
    fn build_module<W, F>(&self, w: &mut Writer<W>, build_templates: F) -> Result<(), Self::Error> where
        W: Write,
        F: FnOnce(&mut Writer<W>) -> Result<(), Self::Error>
    {
        build_templates(w)
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

pub struct Struct {
    pub name: StructName,
    pub fields: HashMap<Name, Type>,
}

#[derive(Clone, Debug)]
pub enum Type {
    Struct(Name, bool),
    Content(bool),
    Bool,
    Collection(Option<Box<Type>>, bool),
}

impl Type {
    pub fn merge_with(&mut self, other: Type) -> Result<Option<(Name, Name)>, Error> {
        let rename = match (self, other) {
            (this, Type::Bool) => {
                this.set_optional(true);
                None
            },
            (this @ &mut Type::Bool, mut other) => {
                other.set_optional(true);
                *this = other;
                None
            },
            (&mut Type::Content(ref mut a_o), Type::Content(ref b_o)) => {
                *a_o = *a_o | *b_o;
                None
            },
            (&mut Type::Collection(ref mut a_inner, ref mut a_o), Type::Collection(ref mut b_inner, ref b_o)) => {
                *a_o = *a_o | *b_o;

                match (a_inner, b_inner.take()) {
                    (&mut Some(ref mut a), ref mut b @ Some(_)) => try!(a.merge_with(*b.take().unwrap())),
                    (a @ &mut None, b) => {
                        *a = b;
                        None
                    },
                    (&mut Some(_), None) => None,
                }
            },
            (&mut Type::Struct(ref mut a_n, ref mut a_o), Type::Struct(ref b_n, ref b_o)) => {
                *a_o = *a_o | *b_o;
                if a_n != b_n {
                    Some((a_n.clone(), b_n.clone()))
                } else {
                    None
                }
            },
            (a, b) => return Err(format!("content cannot be used as both {} and {}", a, b).into())
        };

        Ok(rename)
    }

    pub fn set_optional(&mut self, optional: bool) {
        match *self {
            Type::Struct(_, ref mut o) => *o = optional,
            Type::Content(ref mut o) => *o = optional,
            Type::Collection(_, ref mut o) => *o = optional,
            Type::Bool => {},
        }
    }

    pub fn is_optional(&self) -> bool {
        match *self {
            Type::Struct(_, o) => o,
            Type::Content(o) => o,
            Type::Collection(_, o) => o,
            Type::Bool => false,
        }
    }

    pub fn rename_struct(&mut self, old_name: &Name, new_name: &Name) {
        match *self {
            Type::Struct(ref mut n, _) if n == old_name => *n = new_name.clone(),
            Type::Collection(Some(ref mut inner), _) => inner.rename_struct(old_name, new_name),
            _ => {},
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.is_optional() {
            try!("optional ".fmt(f))
        }

        match *self {
            Type::Content(_) => "content".fmt(f),
            Type::Bool => "boolean value".fmt(f),
            Type::Collection(Some(ref inner), _) => write!(f, "collection of {}", inner),
            Type::Collection(None, _) => "collection of something".fmt(f),
            Type::Struct(ref name, _) => write!(f, "data structure '{}'", name),
        }
    }
}

#[derive(Clone, Debug)]
pub enum StructName {
    Generated(Name),
    Given(Name),
}

impl StructName {
    pub fn is_generated(&self) -> bool {
        match *self {
            StructName::Generated(_) => true,
            StructName::Given(_) => false,
        }
    }

    pub fn is_given(&self) -> bool {
        !self.is_generated()
    }
}

impl Deref for StructName {
    type Target = Name;

    fn deref(&self) -> &Name {
        match *self {
            StructName::Generated(ref n) => n,
            StructName::Given(ref n) => n,
        }
    }
}

impl fmt::Display for StructName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        (**self).fmt(f)
    }
}

impl From<StructName> for Name {
    fn from(name: StructName) -> Name {
        match name {
            StructName::Generated(n) => n,
            StructName::Given(n) => n,
        }
    }
}

impl From<StructName> for String {
    fn from(name: StructName) -> String {
        match name {
            StructName::Generated(n) => n.into(),
            StructName::Given(n) => n.into(),
        }
    }
}

#[derive(Clone, Copy)]
pub struct Structs<'a>(&'a HashMap<Name, Struct>);

impl<'a> Structs<'a> {
    pub fn get<N: Hash + Eq>(&self, name: &N) -> Option<&'a Struct> where Name: Borrow<N> {
        self.0.get(name)
    }
}

impl<'a> From<&'a HashMap<Name, Struct>> for Structs<'a> {
    fn from(structs: &'a HashMap<Name, Struct>) -> Structs<'a> {
        Structs(structs)
    }
}

impl<'a> IntoIterator for Structs<'a> {
    type Item = &'a Struct;
    type IntoIter = Values<'a, Name, Struct>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.values()
    }
}
