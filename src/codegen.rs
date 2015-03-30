use std::io::Write;
use std::collections::HashMap;

use string_cache::atom::Atom;

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
    BeginTag(Atom),
    EndTag(bool),
    CloseTag(Atom),
    BeginAttribute(Atom, Content),
    AppendToAttribute(Content),
    EndAttribute,
    BeginText(Content),
    AppendToText(Content),
    EndText,
    Scope(Scope),
    End,
}

///Representation of supported scope types.
pub enum Scope {
    ///Everything within an `if` scope will be hidden if the provided logic
    ///expression is false.
    If(Logic)
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
    pub fn parameters<F: FnMut(&String)>(&self, f: &mut F) {
        match self {
            &Logic::And(ref conds) => for cond in conds {
                cond.parameters(f);
            },
            &Logic::Or(ref conds) => for cond in conds {
                cond.parameters(f);
            },
            &Logic::Not(ref cond) => cond.parameters(f),
            &Logic::Value(ref name) => f(name)
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
    ///A plain string.
    String,
    ///An optionally defined string.
    OptionalString,
    ///A boolean value.
    Bool
}

///Write `4 * steps` spaces.
#[inline]
pub fn write_indent<W: Write>(writer: &mut W, steps: u8) -> Result<(), Error> {
    for _ in 0..steps {
        try!(write!(writer, "    "));
    }
    Ok(())
}