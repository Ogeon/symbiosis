use std::borrow::Cow;
use std::fmt;

use StrTendril;

use fragment::{FragmentStore, InputType, ReturnType};
use codegen::{ContentType, Path};

use self::slicer::Slicer;

mod slicer;

#[derive(Debug)]
pub enum ErrorKind {
    ExpectedIdentifier(Option<Token>),
    ExpectedString(Option<Token>),
    ExpectedToken(Token, Option<Token>),
    Other(Cow<'static, str>),
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ErrorKind::ExpectedIdentifier(ref token) => {
                try!(f.write_str("expected an identifier, but found "));
                if let Some(ref token) = *token {
                    write!(f, "{}", token)
                } else {
                    f.write_str("nothing")
                }
            },
            ErrorKind::ExpectedString(ref token) => {
                try!(f.write_str("expected a string literal, but found "));
                if let Some(ref token) = *token {
                    write!(f, "{}", token)
                } else {
                    f.write_str("nothing")
                }
            },
            ErrorKind::ExpectedToken(ref expected, ref found) => {
                try!(write!(f, "expected {}, but found ", expected));
                if let Some(ref token) = *found {
                    write!(f, "{}", token)
                } else {
                    f.write_str("nothing")
                }
            },
            ErrorKind::Other(ref e) => e.fmt(f),
        }
    }
}

impl From<&'static str> for ErrorKind {
    fn from(error: &'static str) -> ErrorKind {
        ErrorKind::Other(error.into())
    }
}

impl From<String> for ErrorKind {
    fn from(error: String) -> ErrorKind {
        ErrorKind::Other(error.into())
    }
}

impl From<Cow<'static, str>> for ErrorKind {
    fn from(error: Cow<'static, str>) -> ErrorKind {
        ErrorKind::Other(error)
    }
}

#[derive(Debug)]
pub struct Error {
    callstack: Vec<Cow<'static, str>>,
    kind: ErrorKind
}

impl Error {
    fn add_callee<T: Into<Cow<'static, str>>>(&mut self, callee: T) {
        self.callstack.push(callee.into())
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.callstack.len() > 0 {
            try!(f.write_str("error when calling "));
            for (i, fragment) in self.callstack.iter().enumerate() {
                if i > 0 {
                    try!(f.write_str(" in "));
                }
                try!(write!(f, "'{}'", fragment));
            }
            try!(f.write_str(": "));
        }

        self.kind.fmt(f)
    }
}

impl<T: Into<ErrorKind>> From<T> for Error {
    fn from(error: T) -> Error {
        Error {
            callstack: vec![],
            kind: error.into(),
        }
    }
}

pub fn parse_content(content: StrTendril, fragments: &FragmentStore) -> Result<Vec<ReturnType>, Error> {
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

                    let mut tokenizer = Tokenizer::new(&mut content);
                    let mut parser = Parser {
                        tokenizer: &mut tokenizer,
                        fragments: fragments,
                    };

                    let ret = match try!(parser.outer_fragment()) {
                        ReturnType::Placeholder(ref path, _) if &**path == &["end".into()] => ReturnType::End,
                        ret => ret,
                    };
                    tokens.push(ret);

                    try!(parser.token(Token::CloseBrace).and_then(|_| parser.token(Token::CloseBrace)));
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

fn eval_fragment(name: &str, parser: &mut Parser) -> Result<ReturnType, Error> {
    if let Some(fragment) = parser.fragments.get(name) {
        fragment.process(parser).map_err(|mut e| {
            e.add_callee(fragment.identifier());
            e
        })
    } else {
        Err(format!("'{}' is not a registered fragment", name).into())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Ident(StrTendril),
    Period,
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    String(StrTendril),
    Other(StrTendril),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Token::Ident(ref ident) => write!(f, "the identifier '{}'", ident),
            Token::Period => f.write_str("'.'"),
            Token::OpenParen => f.write_str("'('"),
            Token::CloseParen => f.write_str("')'"),
            Token::OpenBrace => f.write_str("'{'"),
            Token::CloseBrace => f.write_str("'}'"),
            Token::String(ref string) => write!(f, "the string {:?}", &**string),
            Token::Other(ref token) => write!(f, "the token {:?}", &**token),
        }
    }
}

struct Tokenizer<'a> {
    source: &'a mut Slicer,
    peeked: Option<Token>
}

impl<'a> Tokenizer<'a> {
    fn new(source: &mut Slicer) -> Tokenizer {
        Tokenizer {
            source: source,
            peeked: None,
        }
    }

    fn peek(&mut self) -> Option<Token> {
        if self.peeked.is_none() {
            self.peeked = self.next_token();
        }

        self.peeked.clone()
    }

    fn next_token(&mut self) -> Option<Token> {
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
            Some('{') => {
                self.source.discard();
                Some(Token::OpenBrace)
            },
            Some('}') => {
                self.source.discard();
                Some(Token::CloseBrace)
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
            },
            None => None
        }
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        self.peeked.take().or_else(|| self.next_token())
    }
}

fn is_delimiter(c: char) -> bool {
    match c {
        '.' | ':' | ',' | ';' | '(' | ')' | '[' | ']' | '{' | '}' => true,
        _ => false,
    }
}

pub struct Parser<'a>{
    fragments: &'a FragmentStore,
    tokenizer: &'a mut Tokenizer<'a>,
}

impl<'a> Parser<'a> {
    pub fn input(&mut self) -> Result<InputType, Error> {
        let first = try!(self.identifier());
        match self.token(Token::OpenParen) {
            Ok(_) => {
                let input = match try!(eval_fragment(&first, self)) {
                    ReturnType::Placeholder(name, ty) => InputType::Placeholder(name, ty),
                    ReturnType::Logic(cond) => InputType::Logic(cond),
                    _ => return Err("inner fragments must only return placeholders and logic".into())
                };
                self.token(Token::CloseParen).map(|_| input)
            },
            Err(_) => {
                let mut path = vec![first];
                while let Ok(_) = self.token(Token::Period) {
                    path.push(try!(self.identifier()));
                }
                Ok(InputType::Placeholder(path.into(), ContentType::String(false)))
            },
        }
    }

    pub fn string(&mut self) -> Result<StrTendril, Error> {
        match self.tokenizer.peek() {
            Some(Token::String(string)) => {
                self.tokenizer.next();
                Ok(string)
            },
            token => Err(ErrorKind::ExpectedString(token).into()),
        }
    }

    pub fn identifier(&mut self) -> Result<StrTendril, Error> {
        match self.tokenizer.peek() {
            Some(Token::Ident(ident)) => {
                self.tokenizer.next();
                Ok(ident)
            },
            token => Err(ErrorKind::ExpectedIdentifier(token).into()),
        }
    }

    pub fn path(&mut self) -> Result<Path, Error> {
        let mut path = vec![try!(self.identifier())];
        while let Ok(_) = self.token(Token::Period) {
            path.push(try!(self.identifier()));
        }
        Ok(path.into())
    }

    pub fn token(&mut self, token: Token) -> Result<(), Error> {
        match self.tokenizer.peek() {
            Some(ref t) if t == &token => {
                self.tokenizer.next();
                Ok(())
            },
            found => Err(ErrorKind::ExpectedToken(token, found).into()),
        }
    }

    fn outer_fragment(&mut self) -> Result<ReturnType, Error> {
        let first = try!(self.identifier());
        match self.token(Token::OpenParen) {
            Ok(_) => {
                let ret = try!(eval_fragment(&first, self));
                self.token(Token::CloseParen).map(|_| ret)
            },
            Err(_) => {
                let mut path = vec![first];
                while let Ok(_) = self.token(Token::Period) {
                    path.push(try!(self.identifier()));
                }
                Ok(ReturnType::Placeholder(path.into(), ContentType::String(false)))
            },
        }
    }
}
