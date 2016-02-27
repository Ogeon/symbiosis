use std::fmt;
use std::ops::Deref;

use StrTendril;
use string_cache::atom::Atom;
pub use html5ever::tokenizer::Doctype;
pub use symbiosis_core::{Path, ContentType, Logic, Params};

#[derive(Clone, Debug)]
pub enum Name {
    Atom(Atom),
    Str(&'static str),
    Tendril(StrTendril),
}

impl From<Atom> for Name {
    fn from(atom: Atom) -> Name {
        Name::Atom(atom)
    }
}

impl From<&'static str> for Name {
    fn from(string: &'static str) -> Name {
        Name::Str(string)
    }
}

impl From<StrTendril> for Name {
    fn from(tendril: StrTendril) -> Name {
        Name::Tendril(tendril)
    }
}

impl From<String> for Name {
    fn from(string: String) -> Name {
        Name::Tendril(string.into())
    }
}

impl From<Name> for StrTendril {
    fn from(name: Name) -> StrTendril {
        match name {
            Name::Atom(a) => (&*a).into(),
            Name::Str(s) => s.into(),
            Name::Tendril(t) => t,
        }
    }
}

impl Deref for Name {
    type Target = str;

    fn deref(&self) -> &str {
        match *self {
            Name::Atom(ref a) => a,
            Name::Str(s) => s,
            Name::Tendril(ref t) => t,
        }
    }
}

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        (**self).fmt(f)
    }
}

impl AsRef<str> for Name {
    fn as_ref(&self) -> &str {
        self
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Text {
    content: StrTendril,
    escape: bool,
    in_attribute: bool,
}

impl Text {
    pub fn new(content: StrTendril, escape: bool, in_attribute: bool) -> Text {
        Text {
            content: content,
            escape: escape,
            in_attribute: in_attribute,
        }
    }

    pub fn escaped(content: StrTendril, in_attribute: bool) -> Text {
        Text {
            content: content,
            escape: true,
            in_attribute: in_attribute,
        }
    }

    pub fn plain(content: StrTendril) -> Text {
        Text {
            content: content,
            escape: false,
            in_attribute: false,
        }
    }

    pub fn empty() -> Text {
        Text {
            content: StrTendril::new(),
            escape: false,
            in_attribute: false,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.content.len() == 0
    }
}

impl fmt::Display for Text {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.escape {
            for c in self.content.chars() {
                try!(match c {
                    '&' => "&amp;".fmt(f),
                    '\u{00A0}' => "&nbsp;".fmt(f),
                    '"' if self.in_attribute => "&quot;".fmt(f),
                    '<' if !self.in_attribute => "&lt;".fmt(f),
                    '>' if !self.in_attribute => "&gt;".fmt(f),
                    c => write!(f, "{}", c),
                });
            }
            Ok(())
        } else {
            self.content.fmt(f)
        }
    }
}

///Tokens representing different parts of a template document.
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
    TypeHint(Path, ContentType),
}

///Types of text content.
pub enum Content {
    ///A plain string.
    String(Text),

    ///Use content from a parameter in a placeholder.
    Placeholder(Path, ContentType)
}

///Representation of supported scope types.
#[derive(Debug)]
pub enum Scope {
    ///Everything within an `if` scope will be hidden if the provided logic
    ///expression is false.
    If(Logic),
    ///Repeat the content within the scope for each element in a collection.
    ///Args: collection, element, optional key
    ForEach(Path, StrTendril, Option<StrTendril>)
}

