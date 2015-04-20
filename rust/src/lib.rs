//!The common traits and types for Symbiosis templates.

use std::io::{self, Write};
use std::iter::{Iterator, IntoIterator, Enumerate};
use std::fmt;
use std::collections::btree_map::BTreeMap;

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

impl<'a, T: 'a> Collection<'a, &'a fmt::Display> for Vec<T> where
    T: fmt::Display
{
    fn values(&'a self) -> Box<Iterator<Item=&'a fmt::Display> + 'a> {
        Box::new(self.iter().map(|i| i as &fmt::Display))
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

impl<'a, T: 'a> Collection<'a, &'a fmt::Display> for &'a [T] where
    T: fmt::Display
{
    fn values(&'a self) -> Box<Iterator<Item=&'a fmt::Display> + 'a> {
        Box::new(self.into_iter().map(|i| i as &fmt::Display))
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

impl<'a, K, T: 'a> Collection<'a, &'a fmt::Display> for BTreeMap<K, T> where
    K: fmt::Display,
    T: fmt::Display
{
    fn values(&'a self) -> Box<Iterator<Item=&'a fmt::Display> + 'a> {
        Box::new(self.values().map(|i| i as &fmt::Display))
    }

    fn key_values(&'a self) -> KeyValues<'a, &'a fmt::Display> {
        KeyValues::Map(Box::new(self.iter().map(|(k, i)| (k as &fmt::Display, i as &fmt::Display))))
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