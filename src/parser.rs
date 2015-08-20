use std::collections::HashMap;
use std::borrow::{Cow, Borrow};
use std::hash::Hash;

use tendril::StrTendril;

use fragments::{Fragment, InputType, ReturnType};
use codegen::ContentType;

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

pub fn parse_content(content: StrTendril, fragments: &ExtensibleMap<&'static str, Box<Fragment>>) -> Result<Vec<ReturnType>, Cow<'static, str>> {
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

fn parse_fragment(content: &mut Slicer, fragments: &ExtensibleMap<&'static str, Box<Fragment>>) -> Result<ReturnType, Cow<'static, str>> {
    let mut name = StrTendril::new();
    let mut found_fragment = None;
    while let Some(character) = content.next() {
        match character {
            b'(' => {
                name.push_tendril(&content.slice_excluding(1));

                let args = try!(parse_sub_fragments(content, fragments));
                if let Some(fragment) = fragments.get(&*name) {
                    found_fragment = Some(fragment.process(args));
                    name.clear();
                } else {
                    return Err(Cow::Owned(format!("'{}' is not a registered fragment", name)));
                }
            },
            b'}' => match content.next() {
                Some(b'}') => {
                    name.push_tendril(&content.slice_excluding(2));
                    break
                },
                None => {
                    name.push_tendril(&content.slice_excluding(1));
                    break
                },
                _ => {}
            },
            b'\\' => match content.next() {
                Some(b'}') | Some(b'(') => {
                    content.go_back();
                    name.push_tendril(&content.slice_excluding(1));
                    content.next();
                }
                _ => {}
            },
            c if (c as char).is_whitespace() => { //Should be error?
                if content.slice_len() > 1 {
                    name.push_tendril(&content.slice_excluding(1));
                } else {
                    content.discard();
                }
            },
            _ => {}
        }
    }

    if content.slice_len() > 0 {
        name.push_tendril(&content.slice());
    }

    if let Some(result) = found_fragment {
        result
    } else if &*name == "end" {
        Ok(ReturnType::End)
    } else if name.len() > 0 {
        Ok(ReturnType::Placeholder(name, ContentType::String(false)))
    } else {
        Err(Cow::Borrowed("empty fragment"))
    }
}

fn parse_sub_fragments(content: &mut Slicer, fragments: &ExtensibleMap<&'static str, Box<Fragment>>) -> Result<Vec<InputType>, Cow<'static, str>> {
    let mut name = StrTendril::new();
    let mut result = vec![];
    while let Some(character) = content.next() {
        match character {
            b'(' => {
                name.push_tendril(&content.slice_excluding(1));

                let args = try!(parse_sub_fragments(content, fragments));
                if let Some(fragment) = fragments.get(&*name) {
                    result.push(match try!(fragment.process(args)) {
                        ReturnType::Placeholder(name, ty) => InputType::Placeholder(name, ty),
                        ReturnType::Logic(cond) => InputType::Logic(cond),
                        _ => return Err(Cow::Borrowed("inner fragments must only return placeholders and logic"))
                    });
                } else {
                    return Err(Cow::Owned(format!("'{}' is not a registered fragment", name)));
                }
                name.clear();
            },
            b',' => {
                name.push_tendril(&content.slice_excluding(1));
                println!("pushing parameter '{}'", name);
                result.push(InputType::Placeholder(name.clone(), ContentType::String(false)));
                name.clear();
            },
            b')' => {
                name.push_tendril(&content.slice_excluding(1));
                break
            },
            b'\\' => match content.next() {
                Some(b')') | Some(b'(') => {
                    content.go_back();
                    name.push_tendril(&content.slice_excluding(1));
                    content.next();
                },
                _ => {}
            },
            c if (c as char).is_whitespace() => { //Should be error if not after ","?
                if content.slice_len() > 1 {
                    name.push_tendril(&content.slice_excluding(1));
                } else {
                    content.discard();
                }
                println!("found whitespace! name: '{}'", name);
            },
            _ => {println!("'{}' ({})", name, name.len32());}
        }
    }

    if content.slice_len() > 0 {
        name.push_tendril(&content.slice());
    }

    if name.len() > 0 {
        result.push(InputType::Placeholder(name, ContentType::String(false)))
    }

    Ok(result)
}

struct Slicer {
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

    pub fn go_back(&mut self) {
        if self.length > 0 {
            self.length -= 1;
        }
    }

    pub fn slice_len(&self) -> u32 {
        self.length
    }
}
