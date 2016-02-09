use std::collections::HashMap;
use std::borrow::{Cow, Borrow};
use std::hash::Hash;
use std::cmp::min;
use std::fmt;

use tendril::StrTendril;

use fragment::{self, Fragment, InputType, ReturnType};
use codegen::ContentType;

#[derive(Debug)]
pub enum Error {
    Fragment(fragment::Error),
    Other(Cow<'static, str>)
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Error::Fragment(ref e) => write!(f, "during fragment expansion: {}", e),
            Error::Other(ref e) => write!(f, "while parsing: {}", e)
        }
    }
}

impl From<fragment::Error> for Error {
    fn from(error: fragment::Error) -> Error {
        Error::Fragment(error)
    }
}

impl<T: Into<Cow<'static, str>>> From<T> for Error {
    fn from(error: T) -> Error {
        Error::Other(error.into())
    }
}

pub enum ExtensibleMap<'a, K: 'a, V: 'a> {
    Owned(HashMap<K, V>),
    Extended(&'a HashMap<K, V>, HashMap<K, V>)
}

impl<'a, K: Hash + Eq, V> ExtensibleMap<'a, K, V> {
    pub fn extend(base: &'a HashMap<K, V>) -> ExtensibleMap<'a, K, V> {
        ExtensibleMap::Extended(base, HashMap::new())
    }

    pub fn insert(&mut self, key: K, value: V) -> Option<V> {
        match self {
            &mut ExtensibleMap::Owned(ref mut map) => map.insert(key, value),
            &mut ExtensibleMap::Extended(_, ref mut map) => map.insert(key, value),
        }
    }

    pub fn get<Q: ?Sized>(&self, k: &Q) -> Option<&V> where
        K: Borrow<Q>,
        Q: Hash + Eq
    {
        match self {
            &ExtensibleMap::Owned(ref map) => map.get(k),
            &ExtensibleMap::Extended(ref base, ref map) => map.get(k).or_else(|| base.get(k))
        }
    }
}

pub fn parse_content(content: StrTendril, fragments: &ExtensibleMap<&'static str, Box<Fragment>>) -> Result<Vec<ReturnType>, Error> {
    let mut tokens = Vec::new();
    let mut content = Slicer::new(content);

    while let Some(character) = content.next() {
        match character {
            b'{' => match content.next() {
                Some(b'{') => {
                    if content.slice_len() > 2 {
                        tokens.push(ReturnType::String(content.slice_excluding(2)));
                    } else {
                        content.discard();
                    }

                    tokens.push(try!(parse_fragment(&mut content, fragments)));

                    content.skip_whitespace();
                    if !content.eat_bytes(b"}}") {
                        if let Some(c) = content.next_char() {
                            return Err(format!("expected '}}}}', but found '{}'", c).into());
                        } else {
                            return Err("expected '}}', but found nothing".into());
                        }
                    }
                },
                None => break,
                _ => {}
            },
            b'\\' => if let Some(b'{') = content.next() {
                content.go_back();
                tokens.push(ReturnType::String(content.slice_excluding(1)));
                content.next();
            },
            _ => {}
        }
    }

    let remaining = content.remaining();
    if remaining.len() > 0 {
        tokens.push(ReturnType::String(remaining));
    }

    Ok(tokens)
}

fn parse_fragment(content: &mut Slicer, fragments: &ExtensibleMap<&'static str, Box<Fragment>>) -> Result<ReturnType, fragment::Error> {
    content.skip_whitespace();
    if let Some(name) = content.take_while(|c| c == b'_' ||  c == b'.' || (c as char).is_alphabetic()) {
        content.skip_whitespace();
        if content.eat(b'(') {
            if let Some(fragment) = fragments.get(&*name) {
                let pattern = fragment.pattern();
                pattern.parse(content, |src, inner| inner_fragment(src, inner, fragments))
                    .and_then(|args| {
                        content.skip_whitespace();
                        match content.next_char() {
                            Some(')') => fragment.process(args),
                            Some(c) => Err(format!("expected ')', but found {}", c).into()),
                            None => Err("expected ')'".into())
                        }
                    }).map_err(|mut e| {
                        e.add_callee(fragment.identifier());
                        e
                    })
            } else {
                Err(format!("'{}' is not a registered fragment", name).into())
            }
        } else if &*name == "end" {
            Ok(ReturnType::End)
        } else if name.len() > 0 {
            let mut path: Vec<_> = name.split('.').map(From::from).collect();
            Ok(ReturnType::Placeholder(path.into(), ContentType::String(false)))
        } else {
            Err("empty fragment".into())
        }
    } else {
        if let Some(c) = content.next_char() {
            Err(format!("expected an identifier, but found {}", c).into())
        } else {
            Err("expected an identifier".into())
        }
    }
}

fn inner_fragment(content: &mut Slicer, name: StrTendril, fragments: &ExtensibleMap<&'static str, Box<Fragment>>) -> Result<InputType, fragment::Error> {
    if let Some(fragment) = fragments.get(&*name) {
        let pattern = fragment.pattern();
        pattern.parse(content, |src, inner| inner_fragment(src, inner, fragments))
            .and_then(|args| {
                match try!(fragment.process(args)) {
                    ReturnType::Placeholder(name, ty) => Ok(InputType::Placeholder(name, ty)),
                    ReturnType::Logic(cond) => Ok(InputType::Logic(cond)),
                    _ => return Err("inner fragments must only return placeholders and logic".into())
                }
            }).map_err(|mut e| {
                e.add_callee(fragment.identifier());
                e
            })
    } else {
        Err(format!("'{}' is not a registered fragment", name).into())
    }
}

pub struct Slicer {
    src: StrTendril,
    offset: u32,
    length: u32
}

impl Slicer {
    pub fn new(src: StrTendril) -> Slicer {
        Slicer {
            src: src,
            offset: 0,
            length: 0
        }
    }

    pub fn slice(&mut self) -> StrTendril {
        let slice = self.src.subtendril(self.offset, self.length);
        self.offset += self.length;
        self.length = 0;
        slice
    }

    pub fn slice_excluding(&mut self, nbytes: u32) -> StrTendril {
        let len = if self.length >= nbytes {
            self.length - nbytes
        } else {
            0
        };

        let slice = self.src.subtendril(self.offset, len);
        self.offset += self.length;
        self.length = 0;
        slice
    }

    pub fn discard(&mut self) {
        self.offset += self.length;
        self.length = 0;
    }

    pub fn remaining(&mut self) -> StrTendril {
        self.src.subtendril(self.offset, self.src.len32() - self.offset)
    }

    pub fn next(&mut self) -> Option<u8> {
        let index = self.length + self.offset;
        if index < self.src.len32() {
            self.length += 1;
            Some(self.src.as_bytes()[index as usize])
        } else {
            None
        }
    }

    pub fn next_char(&mut self) -> Option<char> {
        let i = (self.offset + self.length) as usize;
        if let Some(c) = self.src[i..].chars().next() {
            self.length += c.len_utf8() as u32;
            Some(c)
        } else {
            None
        }
    }

    pub fn go_back(&mut self) {
        if self.length > 0 {
            self.length -= 1;
        }
    }

    pub fn slice_len(&self) -> u32 {
        self.length
    }

    pub fn offset(&self) -> u32 {
        self.offset
    }

    pub fn jump_to(&mut self, offset: u32) {
        self.offset = min(offset, self.src.len32());
        self.length = 0;
    }

    pub fn skip_whitespace(&mut self) {
        while let Some(c) = self.next() {
            if !(c as char).is_whitespace() {
                self.go_back();
                self.discard();
                break;
            }
        }
    }

    pub fn take_while<F: FnMut(u8) -> bool>(&mut self, mut pred: F) -> Option<StrTendril> {
        while let Some(c) = self.next() {
            if !pred(c) {
                self.go_back();
                return Some(self.slice());
            }
        }

        None
    }

    pub fn eat(&mut self, byte: u8) -> bool {
        match self.next() {
            Some(c) if c == byte => {
                self.discard();
                true
            },
            Some(_) => {
                self.go_back();
                false
            },
            None => false
        }
    }

    pub fn eat_bytes(&mut self, bytes: &[u8]) -> bool {
        let snapshot = self.length;

        for &byte in bytes {
            match self.next() {
                Some(c) => if c != byte {
                    self.length = snapshot;
                    return false;
                },
                None => {
                    self.length = snapshot;
                    return false;
                }
            }
        }

        self.discard();
        
        true
    }
}
