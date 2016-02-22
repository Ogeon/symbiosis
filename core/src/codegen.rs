use std::collections::hash_map::{HashMap, Entry};
use std::fmt;
use std::ops::{Deref, DerefMut};

use tendril::StrTendril;
use string_cache::atom::Atom;
pub use html5ever::tokenizer::Doctype;

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

///Logic expressions.
#[derive(Debug)]
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
}///Types of template parameter content.
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
