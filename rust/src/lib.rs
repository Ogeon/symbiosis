//!The common traits and types for Symbiosis templates.

use std::io::{self, Write};
use std::iter::{Iterator, IntoIterator};
use std::slice::Iter;

///Common trait for Symbiosis templates.
pub trait Template {
    fn render_to(&self, writer: &mut Write) -> io::Result<()>;
}

///A generic collection type.
pub enum Collection<'a, T: 'a> {
    List(&'a [T])
}

impl<'a, T> Collection<'a, T> {
    ///Get an iterator for the values in the collection.
    pub fn values<'i>(&'i self) -> Values<'i, T> {
        match self {
            &Collection::List(ref l) => Values::List(l.into_iter()),
        }
    }
}

///A generic iterator for collection values.
pub enum Values<'a, I: 'a> {
    List(Iter<'a, I>)
}

impl<'a, I> Iterator for Values<'a, I> {
    type Item = &'a I;
    fn next(&mut self) -> Option<Self::Item> {
        match self {
            &mut Values::List(ref mut i) => i.next(),
        }
    }
}