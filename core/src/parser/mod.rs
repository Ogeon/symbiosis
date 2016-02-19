use std::collections::HashMap;
use std::borrow::{Cow, Borrow};
use std::hash::Hash;
use std::cmp::min;
use std::fmt;

use tendril::StrTendril;

use fragment::{self, Fragment, InputType, ReturnType};
use codegen::{ContentType, Path};

use lalrpop_util::ParseError as LalrpopError;

mod grammar;

pub type ParseError = LalrpopError<(), Token, ()>;


#[derive(Debug)]
pub enum Error {
    Fragment(fragment::Error),
    Parse(ParseError),
    Other(Cow<'static, str>),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Error::Fragment(ref e) => write!(f, "during fragment expansion: {}", e),
            Error::Parse(ref e) => match *e {
                LalrpopError::InvalidToken { .. } => write!(f, "parse error: invalid token"),
                LalrpopError::UnrecognizedToken { token: None, .. } => write!(f, "parse error: unrecognized token"),
                LalrpopError::UnrecognizedToken { token: Some((_, ref t, _)), .. } => write!(f, "parse error: unrecognized token {:?}", t),
                LalrpopError::ExtraToken { token: (_, ref t, _), .. } => write!(f, "parse error: extra token {:?}", t),
                LalrpopError::User { ref error } => write!(f, "parse error: {:?}", error),
            },
            Error::Other(ref e) => write!(f, "while parsing: {}", e)
        }
    }
}

impl From<fragment::Error> for Error {
    fn from(error: fragment::Error) -> Error {
        Error::Fragment(error)
    }
}

impl From<ParseError> for Error {
    fn from(error: LalrpopError<(), Token, ()>) -> Error {
        Error::Parse(error)
    }
}

impl From<&'static str> for Error {
    fn from(error: &'static str) -> Error {
        Error::Other(error.into())
    }
}

impl From<String> for Error {
    fn from(error: String) -> Error {
        Error::Other(error.into())
    }
}

impl From<Cow<'static, str>> for Error {
    fn from(error: Cow<'static, str>) -> Error {
        Error::Other(error)
    }
}

pub enum ExtensibleMap<'a, K: 'a, V: 'a> {
    Owned(HashMap<K, V>),
    Extended(&'a HashMap<K, V>, HashMap<K, V>)
}

impl<'a, K: Hash + Eq, V> ExtensibleMap<'a, K, V> {
    pub fn new() -> ExtensibleMap<'a, K, V> {
        ExtensibleMap::Owned(HashMap::new())
    }

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

                    let fragment = try!(grammar::parse_Fragment(Tokenizer::new(&mut content)));
                    let ret = match try!(eval_fragment(&fragment, fragments)) {
                        ReturnType::Placeholder(ref path, _) if &**path == &["end".into()] => ReturnType::End,
                        ret => ret,
                    };
                    tokens.push(ret);

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

fn eval_fragment(fragment: &FragmentKind, fragments: &ExtensibleMap<&'static str, Box<Fragment>>) -> Result<ReturnType, fragment::Error> {
    match *fragment {
        FragmentKind::Function(ref name, ref args) => {
            if let Some(fragment) = fragments.get(&**name) {
                let pattern = fragment.pattern();
                pattern.parse(args, |inner| match try!(eval_fragment(inner, fragments)) {
                        ReturnType::Placeholder(name, ty) => Ok(InputType::Placeholder(name, ty)),
                        ReturnType::Logic(cond) => Ok(InputType::Logic(cond)),
                        _ => Err("inner fragments must only return placeholders and logic".into())
                    })
                    .and_then(|args| fragment.process(args))
                    .map_err(|mut e| {
                        e.add_callee(fragment.identifier());
                        e
                    })
            } else {
                Err(format!("'{}' is not a registered fragment", name).into())
            }
        },
        FragmentKind::Placeholder(ref path) => Ok(ReturnType::Placeholder(path.clone(), ContentType::String(false)))
    }
}

#[derive(Debug, Clone)]
pub enum FragmentKind {
    Placeholder(Path),
    Function(StrTendril, Vec<Input>),
}

#[derive(Debug, Clone)]
pub enum Token {
    Ident(StrTendril),
    Period,
    OpenParen,
    CloseParen,
    String(StrTendril),
    Other(StrTendril),
    FragmentEnd,
}

#[derive(Debug, Clone)]
pub enum Input {
    Fragment(FragmentKind),
    String(StrTendril),
    Other(StrTendril),
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

    pub fn jump_back_by(&mut self, offset: u32) {
        if offset > self.offset {
            self.offset = 0;
        } else {
            self.offset -= offset;
        }
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

struct Tokenizer<'a> {
    source: &'a mut Slicer,
}

impl<'a> Tokenizer<'a> {
    fn new(source: &mut Slicer) -> Tokenizer {
        Tokenizer {
            source: source
        }
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        self.source.skip_whitespace();

        match self.source.next_char() {
            Some('.') => {
                self.source.discard();
                Some(Token::Period)
            },
            Some('(') => {
                self.source.discard();
                Some(Token::OpenParen)
            },
            Some(')') => {
                self.source.discard();
                Some(Token::CloseParen)
            },
            Some('"') => {
                let mut escaped = false;
                let mut skip_last = false;
                self.source.discard();

                while let Some(next) = self.source.next_char() {
                    match next {
                        '\\' => {
                            self.source.next_char();
                            escaped = true;
                        },
                        '"' => {
                            skip_last = true;
                            break;
                        },
                        _ => {},
                    }
                }

                let content = if skip_last {
                    self.source.slice_excluding(1)
                } else {
                    self.source.slice()
                };

                if escaped {
                    let mut buffer = String::with_capacity(content.len());
                    let mut chars = content.chars();
                    while let Some(next) = chars.next() {
                        match next {
                            '\\' => if let Some(next) = chars.next() { buffer.push(next) },
                            next => buffer.push(next),
                        }
                    }
                    Some(Token::String(buffer.into()))
                } else {
                    Some(Token::String(content))
                }
            },
            Some(c) => {
                if c == '}' && self.source.eat(b'}') {
                    self.source.jump_back_by(2);
                    None
                } else {
                    let mut is_ident = c.is_alphabetic() || c == '_';
                    let token = self.source.take_while(|c| {
                        let c = c as char; //not necessarily optimal
                        if !is_delimiter(c) && !c.is_whitespace() {
                            is_ident = is_ident && (c.is_alphabetic() || c.is_numeric() || c == '_');
                            true
                        } else {
                            false
                        }
                    });

                    token.map(|t| if is_ident {
                        Token::Ident(t)
                    } else {
                        Token::Other(t)
                    })
                }
            },
            None => None 
        }
    }
}

fn is_delimiter(c: char) -> bool {
    match c {
        '.' | ':' | ',' | ';' | '(' | ')' | '[' | ']' | '{' | '}' => true,
        _ => false,
    }
}
