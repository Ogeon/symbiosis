use std::io::{self, Write};
use std::collections::hash_map::{HashMap, Entry};
use std::fmt;
use std::ops::{Deref, DerefMut};

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
    fn build_template<W: Write>(&self, w: &mut Writer<W>, name: &str, params: &Params, tokens: &[Token]) -> Result<(), Self::Error>;

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
    ForEach(Path, StrTendril, Option<StrTendril>)
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
    Value(Path)
}

impl Logic {
    pub fn placeholders(&self) -> Vec<&Path> {
        let mut res = vec![];
        self.placeholders_r(&mut res);
        res
    }

    fn placeholders_r<'a>(&'a self, res: &mut Vec<&'a Path>) {
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
    Placeholder(Path)
}

///Types of template parameter content.
#[derive(Clone, Debug)]
pub enum ContentType {
    ///A plain (maybe optional) string.
    String(bool),
    ///A boolean value.
    Bool,
    ///A (maybe optional) collection of hopefully inferred content, associated with a key.
    Collection(Option<Box<ContentType>>, bool),
    ///A (maybe optional) data structure.
    Struct(Option<String>, Params, bool),
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
            (&mut ContentType::Collection(ref mut a, ref mut a_o), ContentType::Collection(ref mut b, ref mut b_o)) => {
                *a_o = *a_o | *b_o;
                match (a, b.take()) {
                    (&mut Some(ref mut a), ref mut b @ Some(_)) => try!(a.combine_with(*b.take().unwrap())),
                    (a @ &mut None, b @ Some(_)) => *a = b,
                    (&mut Some(_), None) |(&mut None, None)  => {}
                };
            },
            (&mut ContentType::Struct(_, ref mut a, ref mut a_o), ContentType::Struct(_, ref mut b, ref mut b_o)) => {
                *a_o = *a_o | *b_o;

                for (name, ty) in b.drain() {
                    match a.entry(name) {
                        Entry::Occupied(mut e) => try!(e.get_mut().combine_with(ty)),
                        Entry::Vacant(e) => {e.insert(ty);},
                    }
                }
            },
            (a, b) => return Err(format!("content cannot be used as both {} and {}", a, b))
        }

        Ok(())
    }

    pub fn is_optional(&self) -> bool {
        match self {
            &ContentType::String(optional) => optional,
            &ContentType::Bool => false,
            &ContentType::Collection(_, optional) => optional,
            &ContentType::Struct(_, _, optional) => optional,
        }
    }

    pub fn set_optional(&mut self, optional: bool) {
        match self {
            &mut ContentType::String(ref mut o) => *o = optional,
            &mut ContentType::Bool => {},
            &mut ContentType::Collection(_, ref mut o) => *o = optional,
            &mut ContentType::Struct(_, _, ref mut o) => *o = optional,
        }
    }

    pub fn shallow_clone(&self) -> ContentType {
        match *self {
            ContentType::String(o) => ContentType::String(o),
            ContentType::Bool => ContentType::Bool,
            ContentType::Collection(Some(ref inner), o) => ContentType::Collection(Some(Box::new(inner.shallow_clone())), o),
            ContentType::Collection(None, o) => ContentType::Collection(None, o),
            ContentType::Struct(_, _, o) => ContentType::Struct(None, Params::new(), o),
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
            &ContentType::Collection(Some(ref a), _) => write!(f, "collection of {}", a),
            &ContentType::Collection(None, _) => "collection of something".fmt(f),
            &ContentType::Struct(_, _, _) => "data structure".fmt(f),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Path(Vec<StrTendril>);

impl Path {
    pub fn extend<I: IntoIterator<Item=StrTendril>>(&mut self, iter: I) {
        self.0.extend(iter);
    }
}

impl Deref for Path {
    type Target = [StrTendril];

    fn deref(&self) -> &[StrTendril] {
        &self.0
    }
}

impl fmt::Display for Path {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut parts = self.iter();

        if let Some(first) = parts.next() {
            try!(first.fmt(f));
        }

        for part in parts {
            try!(write!(f, ".{}", part));
        }

        Ok(())
    }
}

impl IntoIterator for Path {
    type IntoIter = <Vec<StrTendril> as IntoIterator>::IntoIter;
    type Item = StrTendril;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a> IntoIterator for &'a Path {
    type IntoIter = <&'a Vec<StrTendril> as IntoIterator>::IntoIter;
    type Item = &'a StrTendril;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl From<Vec<StrTendril>> for Path {
    fn from(path: Vec<StrTendril>) -> Path {
        Path(path)
    }
}

impl From<StrTendril> for Path {
    fn from(path: StrTendril) -> Path {
        Path(vec![path])
    }
}

#[derive(Clone, Debug)]
pub struct Params(HashMap<StrTendril, ContentType>);

impl Params {
    pub fn new() -> Params {
        Params(HashMap::new())
    }

    pub fn get(&self, path: &Path) -> Option<&ContentType> {
        let mut current_params = Some(&self.0);
        let mut current_type = None;

        for part in path {
            if let Some(params) = current_params.take() {
                current_type = params.get(part);
                if let Some(&ContentType::Struct(_, ref params, _)) = current_type {
                    current_params = Some(params);
                }
            } else {
                return None;
            }
        }

        println!("searching for {} and found {:?}", path, current_type);

        current_type
    }

    pub fn flatten(&self, main_struct_name: String) -> HashMap<String, HashMap<StrTendril, ContentType>> {
        let mut structs = HashMap::new();
        let mut stack = vec![(main_struct_name, &self.0)];

        while let Some((name, entries)) = stack.pop() {
            let shallow_entries = structs.entry(name.clone()).or_insert(HashMap::new());

            for (property, ty) in entries {
                let ty = match *ty {
                    ContentType::Struct(ref struct_name, ref entries, optional) => {
                        let struct_name = struct_name.as_ref().cloned().unwrap_or_else(|| {
                            let mut n = name.clone();
                            n.push_str(&property);
                            n.into()
                        });

                        stack.push((struct_name.clone(), entries));
                        ContentType::Struct(Some(struct_name), Params::new(), optional)
                    },
                    ContentType::Collection(ref inner, optional) => {
                        let mut inner_optional = vec![];
                        let mut inner_type = None;
                        {
                            let mut current = inner;

                            while let &Some(ref inner) = current {
                                match **inner {
                                    ContentType::Struct(ref struct_name, ref entries, optional) => {
                                        let struct_name = struct_name.as_ref().cloned().unwrap_or_else(|| {
                                            let mut n = name.clone();
                                            n.push_str(&property);
                                            n.into()
                                        });

                                        stack.push((struct_name.clone(), entries));
                                        inner_type = Some(Box::new(ContentType::Struct(Some(struct_name), Params::new(), optional)));
                                        break;
                                    },
                                    ContentType::Collection(ref inner, optional) => {
                                        current = inner;
                                        inner_optional.push(optional);
                                    },
                                    ref ty => {
                                        inner_type = Some(Box::new(ty.clone()));
                                        break;
                                    },
                                }
                            }
                        }

                        for optional in inner_optional.into_iter().rev() {
                            let new_inner = Some(Box::new(ContentType::Collection(inner_type.take(), optional)));
                            ::std::mem::replace(&mut inner_type, new_inner);
                        }

                        ContentType::Collection(inner_type, optional)
                    },
                    ref ty => ty.clone(),
                };

                shallow_entries.insert(property.clone(), ty);
            }
        }

        structs
    }
}

impl Deref for Params {
    type Target = HashMap<StrTendril, ContentType>;

    fn deref(&self) -> &HashMap<StrTendril, ContentType> {
        &self.0
    }
}

impl DerefMut for Params {
    fn deref_mut(&mut self) -> &mut HashMap<StrTendril, ContentType> {
        &mut self.0
    }
}

impl<'a> IntoIterator for &'a Params {
    type IntoIter = <&'a HashMap<StrTendril, ContentType> as IntoIterator>::IntoIter;
    type Item = (&'a StrTendril, &'a ContentType);

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
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