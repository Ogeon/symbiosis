//!The common traits and types for Symbiosis templates.

use std::io::{self, Write};
use std::iter::{Iterator, IntoIterator, Enumerate};
use std::slice;
use std::fmt;
use std::collections::btree_map::{self, BTreeMap};

///Common trait for Symbiosis templates.
pub trait Template {
    fn render_to(&self, writer: &mut Write) -> io::Result<()>;
}

///A generic collection type.
pub enum Collection<'a, T: 'a> {
    List(&'a [T]),
    Map(&'a BTreeMap<String, T>)
}

impl<'a, T> From<&'a [T]> for Collection<'a, T> {
    fn from(other: &'a [T]) -> Collection<'a, T> {
        Collection::List(other)
    }
}

impl<'a, T> From<&'a BTreeMap<String, T>> for Collection<'a, T> {
    fn from(other: &'a BTreeMap<String, T>) -> Collection<'a, T> {
        Collection::Map(other)
    }
}

impl<'a, T> Collection<'a, T> {
    ///Get an iterator for the values in the collection.
    pub fn values<'i>(&'i self) -> Values<'i, T> {
        match self {
            &Collection::List(ref l) => Values::List(l.into_iter()),
            &Collection::Map(ref m) => Values::Map(m.values())
        }
    }

    ///Get an iterator for the keys and values in the collection.
    pub fn key_values<'i>(&'i self) -> KeyValues<'i, T> {
        match self {
            &Collection::List(ref l) => KeyValues::List(l.into_iter().enumerate()),
            &Collection::Map(ref m) => KeyValues::Map(m.iter())
        }
    }
}

///A generic iterator for collection values.
pub enum Values<'a, I: 'a> {
    List(slice::Iter<'a, I>),
    Map(btree_map::Values<'a, String, I>)
}

impl<'a, I> Iterator for Values<'a, I> {
    type Item = &'a I;
    fn next(&mut self) -> Option<Self::Item> {
        match self {
            &mut Values::List(ref mut i) => i.next(),
            &mut Values::Map(ref mut i) => i.next()
        }
    }
}

///A generic iterator for collection keys and values.
///List indices will start from `1`, for aesthetic reasons.
pub enum KeyValues<'a, I: 'a> {
    List(Enumerate<slice::Iter<'a, I>>),
    Map(btree_map::Iter<'a, String, I>)
}

impl<'a, I> Iterator for KeyValues<'a, I> {
    type Item = (Key<'a>, &'a I);
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
    Map(&'a String)
}

impl<'a> fmt::Display for Key<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Key::List(i) => i.fmt(f),
            &Key::Map(k) => k.fmt(f)
        }
    }
}