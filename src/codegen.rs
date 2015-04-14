use std::io::Write;
use std::collections::HashMap;
use std::borrow::Cow;
use std::fmt;

use string_cache::atom::Atom;
use html5ever::tokenizer::Doctype;

use Error;

///Write an indented line of code.
///
///Example: `line!(writer, indent_steps, "var {} = this.{};", var_name, field_name);`.
#[macro_export]
macro_rules! line {
    ($writer:ident, $indent:expr, $($format:tt)*) => (
        {
            try!($crate::codegen::write_indent($writer, $indent));
            try!(writeln!($writer, $($format)*));
        }
    )
}

///Code generators are used to generate template code in different languages
///and they have to implement this trait.
pub trait Codegen {
    ///Generate code for a single template.
    fn build_template<W: Write>(&self, w: &mut W, name: &str, indent: u8, params: &HashMap<String, ContentType>, tokens: &[Token]) -> Result<(), Error>;

    ///Generate code for a module or a similar collection containing multiple templates.
    fn build_module<W, F>(&self, w: &mut W, build_templates: F) -> Result<(), Error> where
        W: Write,
        F: FnOnce(&mut W, u8) -> Result<(), Error>
    {
        build_templates(w, 0)
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
    ForEach(String, String, Option<String>)
}

///Logic expressions.
pub enum Logic {
    ///a and b and ...
    And(Vec<Logic>),
    ///a or b or ...
    Or(Vec<Logic>),
    ///The logical inverse of an expression.
    Not(Box<Logic>),
    ///A value from a template parameter.
    Value(String)
}

impl Logic {
    pub fn placeholders(&self) -> Vec<&str> {
        let mut res = vec![];
        self.placeholders_r(&mut res);
        res
    }

    fn placeholders_r<'a>(&'a self, res: &mut Vec<&'a str>) {
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
    String(String),

    ///Use content from a parameter in a placeholder.
    Placeholder(String)
}

///Types of template parameter content.
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
    pub fn combine_with(&mut self, pref_ty: ContentType) -> Result<(), Cow<'static, str>> {
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
            (a, b) => return Err(Cow::Owned(format!("content cannot be used as both {} and {}", a, b)))
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
            &ContentType::Collection(None, _) => "collection of unknown content type".fmt(f),
        }
    }
}

///Write `4 * steps` spaces.
#[inline]
pub fn write_indent<W: Write>(writer: &mut W, steps: u8) -> Result<(), Error> {
    for _ in 0..steps {
        try!(write!(writer, "    "));
    }
    Ok(())
}