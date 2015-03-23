use std::str::Chars;
use std::collections::HashMap;
use std::borrow::{Cow, Borrow};
use std::hash::Hash;

use fragments::{Fragment, InputType, ReturnType};
use codegen::Content;

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

pub fn parse_content(content: &str, fragments: &ExtensibleMap<&'static str, Box<Fragment>>) -> Result<Vec<ReturnType>, Cow<'static, str>> {
    let mut buf = String::new();
    let mut tokens = Vec::new();
    let mut content = content.chars();

    while let Some(character) = content.next() {
        match character {
            '{' => match content.next() {
                Some('{') => {
                    if buf.len() > 0 {
                        tokens.push(ReturnType::Content(Content::String(buf)));
                        buf = String::new();
                    }

                    tokens.push(try!(parse_fragment(&mut content, fragments)));
                },
                Some(c) => {
                    buf.push('{');
                    buf.push(c);
                },
                None => break
            },
            '\n' => buf.push_str("\\n"),
            '\t' => buf.push_str("\\t"),
            c => buf.push(c)
        }
    }

    if buf.len() > 0 {
        tokens.push(ReturnType::Content(Content::String(buf)));
    }

    Ok(tokens)
}

fn parse_fragment(content: &mut Chars, fragments: &ExtensibleMap<&'static str, Box<Fragment>>) -> Result<ReturnType, Cow<'static, str>> {
    let mut buf = String::new();
    let mut found_fragment = None;
    while let Some(character) = content.next() {
        match character {
            '(' => {
                let args = try!(parse_sub_fragments(content, fragments));
                if let Some(fragment) = fragments.get(&buf[..]) {
                    found_fragment = Some(fragment.process(args));
                    buf.clear();
                } else {
                    return Err(Cow::Owned(format!("'{}' is not a registered fragment", buf)));
                }
            },
            '}' => match content.next() {
                Some('}') | None => break,
                Some(c) => {
                    buf.push('}');
                    buf.push(c);
                }
            },
            c if c.is_whitespace() => {},
            c => buf.push(c)
        }
    }

    if let Some(result) = found_fragment {
        result
    } else if &buf[..] == "end" {
        Ok(ReturnType::End)
    } else if buf.len() > 0 {
        Ok(ReturnType::Content(Content::Placeholder(buf)))
    } else {
        Err(Cow::Borrowed("empty fragment"))
    }
}

fn parse_sub_fragments(content: &mut Chars, fragments: &ExtensibleMap<&'static str, Box<Fragment>>) -> Result<Vec<InputType>, Cow<'static, str>> {
    let mut buf = String::new();
    let mut result = vec![];
    while let Some(character) = content.next() {
        match character {
            '(' => {
                let args = try!(parse_sub_fragments(content, fragments));
                if let Some(fragment) = fragments.get(&buf[..]) {
                    result.push(match try!(fragment.process(args)) {
                        ReturnType::Content(val) => InputType::Content(val),
                        ReturnType::Logic(cond) => InputType::Logic(cond),
                        _ => return Err(Cow::Borrowed("inner fragments must only return content and logic"))
                    });
                } else {
                    return Err(Cow::Owned(format!("'{}' is not a registered fragment", buf)));
                }
                buf.clear();
            },
            ',' => if buf.len() > 0 {
                result.push(InputType::Content(Content::Placeholder(buf.clone())));
                buf.clear();
            },
            ')' => break,
            c if c.is_whitespace() => {},
            c => buf.push(c)
        }
    }

    if buf.len() > 0 {
        result.push(InputType::Content(Content::Placeholder(buf)))
    }

    Ok(result)
}