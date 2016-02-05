//!The common traits and types for Symbiosis templates.

use std::iter::{Iterator, IntoIterator, Enumerate};
use std::fmt;
use std::collections::btree_map::BTreeMap;
use std::borrow::Cow;

macro_rules! impl_content {
    (int $($t: ty),+) => ($(
        impl<'a> From<$t> for Content<'a> {
            fn from(i: $t) -> Content<'a> {
                Content::Int(i as i64)
            }
        }
    )+);
    (uint $($t: ty),+) => ($(
        impl<'a> From<$t> for Content<'a> {
            fn from(u: $t) -> Content<'a> {
                Content::Uint(u as u64)
            }
        }
    )+);
    (float $($t: ty),+) => ($(
        impl<'a> From<$t> for Content<'a> {
            fn from(f: $t) -> Content<'a> {
                Content::Float(f as f64)
            }
        }
    )+);
}

pub enum Content<'a> {
    String(String),
    Str(&'a str),
    Int(i64),
    Uint(u64),
    Float(f64),
    Display(Box<fmt::Display + 'a>),
    DisplayRef(&'a fmt::Display),
}

impl<'a> fmt::Display for Content<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Content::String(ref a) => a.fmt(f),
            Content::Str(a) => a.fmt(f),
            Content::Int(a) => a.fmt(f),
            Content::Uint(a) => a.fmt(f),
            Content::Float(a) => a.fmt(f),
            Content::Display(ref a) => a.fmt(f),
            Content::DisplayRef(a) => a.fmt(f),
        }
    }
}

impl<'a> From<String> for Content<'a> {
    fn from(s: String) -> Content<'a> {
        Content::String(s)
    }
}

impl<'a> From<&'a str> for Content<'a> {
    fn from(s: &'a str) -> Content<'a> {
        Content::Str(s)
    }
}

impl<'a> From<Cow<'a, str>> for Content<'a> {
    fn from(c: Cow<'a, str>) -> Content<'a> {
        match c {
            Cow::Owned(s) => Content::String(s),
            Cow::Borrowed(s) => Content::Str(s),
        }
    }
}

impl<'a, D: fmt::Display + 'a> From<Box<D>> for Content<'a> {
    fn from(d: Box<D>) -> Content<'a> {
        Content::Display(d as Box<fmt::Display + 'a>)
    }
}

impl<'a, D: fmt::Display + 'a> From<&'a D> for Content<'a> {
    fn from(d: &'a D) -> Content<'a> {
        Content::DisplayRef(d as &'a fmt::Display)
    }
}

impl_content!(int i8, i16, i32, i64, isize);
impl_content!(uint u8, u16, u32, u64, usize);
impl_content!(float f32, f64);

///Common trait for Symbiosis templates.
pub trait Template {
    fn render_to(&self, writer: &mut fmt::Write) -> fmt::Result;
}

///A sequence of templates for lazy conversion from collection item to template.
pub struct Templates<'a, I: 'a + ?Sized, F> where
    &'a I: IntoIterator
{
    content: &'a I,
    as_template: F
}

impl <'a, I: 'a + ?Sized, F, T> Templates<'a, I, F> where
    &'a I: IntoIterator,
    F: Fn(<&'a I as IntoIterator>::Item) -> T,
    T: Template
{
    pub fn new(content: &'a I, as_template: F) -> Templates<'a, I, F> {
        Templates {
            content: content,
            as_template: as_template
        }
    }
}


impl<'a, I: 'a + ?Sized, F, T> Template for Templates<'a, I, F> where
    &'a I: IntoIterator,
    F: Fn(<&'a I as IntoIterator>::Item) -> T,
    T: Template
{
    fn render_to(&self, writer: &mut fmt::Write) -> fmt::Result {
        for post in self.content {
            try!((self.as_template)(post).render_to(writer))
        }

        Ok(())
    }
}

///A generic iterator for collection keys and values.
///List indices will start from `1`, for aesthetic reasons.
pub enum KeyValues<'a, I: 'a> {
    List(Enumerate<Box<Iterator<Item=I> + 'a>>),
    Map(Box<Iterator<Item=(&'a fmt::Display, I)> + 'a>)
}

impl<'a, I> Iterator for KeyValues<'a, I> {
    type Item = (Key<'a>, I);
    fn next(&mut self) -> Option<Self::Item> {
        match self {
            &mut KeyValues::List(ref mut i) => i.next().map(|(k, v)| (Key::List(k + 1), v)),
            &mut KeyValues::Map(ref mut i) => i.next().map(|(k, v)| (Key::Map(k), v))
        }
    }
}

///A generic collection key.
pub enum Key<'a> {
    List(usize),
    Map(&'a fmt::Display)
}

impl<'a> fmt::Display for Key<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Key::List(i) => i.fmt(f),
            &Key::Map(k) => k.fmt(f)
        }
    }
}

pub enum Collection<'a, T: 'a> {
    Slice(&'a [T]),
    Vec(Vec<T>),
    Map(BTreeMap<String, T>),
}

impl<'a, T> Collection<'a, T> {
    ///Get an iterator for the values in the collection.
    pub fn values<'b>(&'b self) -> Box<Iterator<Item=&T> + 'b> {
        match *self {
            Collection::Slice(s) => Box::new(s.iter()),
            Collection::Vec(ref v) => Box::new(v.iter()),
            Collection::Map(ref m) => Box::new(m.values()),
        }
    }
    
    ///Get an iterator for the keys and values in the collection.
    pub fn key_values(&self) -> KeyValues<'a, &T> {
        match *self {
            Collection::Slice(s) => KeyValues::List((Box::new(s.iter()) as Box<Iterator<Item=_>>).enumerate()),
            Collection::Vec(ref v) => KeyValues::List((Box::new(v.iter()) as Box<Iterator<Item=_>>).enumerate()),
            Collection::Map(ref m) => KeyValues::Map(Box::new(m.iter().map(|(k, i)| (k as &fmt::Display, i)))),
        }
    }
}

impl<'a, T> From<&'a [T]> for Collection<'a, T> {
    fn from(slice: &'a [T]) -> Collection<'a, T> {
        Collection::Slice(slice)
    }
}

impl<'a, T> From<Vec<T>> for Collection<'a, T> {
    fn from(vec: Vec<T>) -> Collection<'a, T> {
        Collection::Vec(vec)
    }
}

impl<'a, T> From<BTreeMap<String, T>> for Collection<'a, T> {
    fn from(map: BTreeMap<String, T>) -> Collection<'a, T> {
        Collection::Map(map)
    }
}
