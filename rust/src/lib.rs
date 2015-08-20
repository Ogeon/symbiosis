//!The common traits and types for Symbiosis templates.

use std::io::{self, Write};
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
    Cow(Cow<'a, str>),
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
            Content::Cow(ref a) => a.fmt(f),
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
        Content::Cow(c)
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
    fn render_to(&self, writer: &mut Write) -> io::Result<()>;
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
    fn render_to(&self, writer: &mut Write) -> io::Result<()> {
        for post in self.content {
            try!((self.as_template)(post).render_to(writer))
        }

        Ok(())
    }
}

///A generic collection trait.
pub trait Collection<'a, T: 'a> {
    ///Get an iterator for the values in the collection.
    fn values(&'a self) -> Box<Iterator<Item=T> + 'a>;
    
    ///Get an iterator for the keys and values in the collection.
    fn key_values(&'a self) -> KeyValues<'a, T> {
        KeyValues::List(self.values().enumerate())
    }
}

impl<'a, T: 'a, C> Collection<'a, T> for &'a C where
    C: Collection<'a, T>
{
    fn values(&'a self) -> Box<Iterator<Item=T> + 'a> {
        (*self).values()
    }

    fn key_values(&'a self) -> KeyValues<'a, T> {
        (*self).key_values()
    }
}


impl<'a, T: 'a> Collection<'a, &'a T> for Vec<T> {
    fn values(&'a self) -> Box<Iterator<Item=&'a T> + 'a> {
        Box::new(self.iter())
    }
}

impl<'a, T: 'a> Collection<'a, &'a Template> for Vec<T> where
    T: Template
{
    fn values(&'a self) -> Box<Iterator<Item=&'a Template> + 'a> {
        Box::new(self.iter().map(|i| i as &Template))
    }
}

impl<'a, T: 'a> Collection<'a, Content<'a>> for Vec<T> where
    &'a T: Into<Content<'a>>
{
    fn values(&'a self) -> Box<Iterator<Item=Content<'a>> + 'a> {
        Box::new(self.iter().map(|i| i.into()))
    }
}

impl<'a, T: 'a, C> Collection<'a, &'a Collection<'a, T>> for Vec<C> where
    C: Collection<'a, T>
{
    fn values(&'a self) -> Box<Iterator<Item=&'a Collection<'a, T>> + 'a> {
        Box::new(self.iter().map(|i| i as &Collection<'a, T>))
    }
}


impl<'a, T: 'a> Collection<'a, &'a T> for &'a [T] {
    fn values(&'a self) -> Box<Iterator<Item=&'a T> + 'a> {
        Box::new(self.into_iter())
    }
}

impl<'a, T: 'a> Collection<'a, &'a Template> for &'a [T] where
    T: Template
{
    fn values(&'a self) -> Box<Iterator<Item=&'a Template> + 'a> {
        Box::new(self.into_iter().map(|i| i as &Template))
    }
}

impl<'a, T: 'a> Collection<'a, Content<'a>> for &'a [T] where
    &'a T: Into<Content<'a>>
{
    fn values(&'a self) -> Box<Iterator<Item=Content<'a>> + 'a> {
        Box::new(self.into_iter().map(|i| i.into()))
    }
}

impl<'a, T: 'a, C> Collection<'a, &'a Collection<'a, T>> for &'a [C]  where
    C: Collection<'a, T>
{
    fn values(&'a self) -> Box<Iterator<Item=&'a Collection<'a, T>> + 'a> {
        Box::new(self.into_iter().map(|i| i as &Collection<'a, T>))
    }
}


impl<'a, K, T: 'a> Collection<'a, &'a T> for BTreeMap<K, T> where
    K: fmt::Display
{
    fn values(&'a self) -> Box<Iterator<Item=&'a T> + 'a> {
        Box::new(self.values())
    }

    fn key_values(&'a self) -> KeyValues<'a, &'a T> {
        KeyValues::Map(Box::new(self.iter().map(|(k, i)| (k as &fmt::Display, i))))
    }
}

impl<'a, K, T: 'a> Collection<'a, &'a Template> for BTreeMap<K, T> where
    K: fmt::Display,
    T: Template
{
    fn values(&'a self) -> Box<Iterator<Item=&'a Template> + 'a> {
        Box::new(self.values().map(|i| i as &Template))
    }

    fn key_values(&'a self) -> KeyValues<'a, &'a Template> {
        KeyValues::Map(Box::new(self.iter().map(|(k, i)| (k as &fmt::Display, i as &Template))))
    }
}

impl<'a, K, T: 'a> Collection<'a, Content<'a>> for BTreeMap<K, T> where
    K: fmt::Display,
    &'a T: Into<Content<'a>>
{
    fn values(&'a self) -> Box<Iterator<Item=Content<'a>> + 'a> {
        Box::new(self.values().map(|i| i.into()))
    }

    fn key_values(&'a self) -> KeyValues<'a, Content<'a>> {
        KeyValues::Map(Box::new(self.iter().map(|(k, i)| (k as &fmt::Display, i.into()))))
    }
}

impl<'a, K, T: 'a, C> Collection<'a, &'a Collection<'a, T>> for BTreeMap<K, C> where
    K: fmt::Display,
    C: Collection<'a, T>
{
    fn values(&'a self) -> Box<Iterator<Item=&'a Collection<'a, T>> + 'a> {
        Box::new(self.values().map(|i| i as &Collection<'a, T>))
    }

    fn key_values(&'a self) -> KeyValues<'a, &'a Collection<'a, T>> {
        KeyValues::Map(Box::new(self.iter().map(|(k, i)| (k as &fmt::Display, i as &Collection<'a, T>))))
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