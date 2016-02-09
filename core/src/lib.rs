extern crate tendril;
extern crate html5ever;
extern crate string_cache;

use std::mem::replace;
use std::fmt;
use std::io;

pub use tendril::StrTendril;

use html5ever::Attribute;
use html5ever::tokenizer::{Tag, TagKind, Doctype};
use html5ever::tokenizer::Token as HtmlToken;

use string_cache::Atom;

use codegen::{Token, Content};
use parser::ExtensibleMap;
use fragment::{Fragment, ReturnType};

pub mod codegen;
pub mod parser;
pub mod fragment;

#[derive(Debug)]
pub enum Error {
    Io(io::Error),
    Format(fmt::Error),
    Parse(Vec<parser::Error>)
}

impl From<io::Error> for Error {
    fn from(e: io::Error) -> Error {
        Error::Io(e)
    }
}

impl From<fmt::Error> for Error {
    fn from(e: fmt::Error) -> Error {
        Error::Format(e)
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Error::Io(ref e) => e.fmt(f),
            &Error::Format(ref e) => e.fmt(f),
            &Error::Parse(ref e) => {
                try!(write!(f, "parse errors:\n"));
                for error in e.iter() {
                    try!(write!(f, " - {}\n", error));
                }
                Ok(())
            }
        }
    }
}

pub struct Tokenizer<T: TokenSink> {
    sink: T,
    errors: Vec<parser::Error>,
}

impl<T: TokenSink> Tokenizer<T> {
    pub fn new(sink: T) -> Tokenizer<T> {
        Tokenizer {
            sink: sink,
            errors: vec![],
        }
    }

    pub fn parse_string(&mut self, source: String) -> Result<(), Error> {
        {
            let mut tokenizer = html5ever::tokenizer::Tokenizer::new(&mut*self, Default::default());
            tokenizer.feed(source.into());
        }

        if !self.errors.is_empty() {
            Err(Error::Parse(replace(&mut self.errors, vec![])))
        } else {
            Ok(())
        }
    }

    fn set_doctype(&mut self, docktype: Doctype) {
        self.sink.process_token(Token::SetDoctype(docktype));
    }

    fn add_text(&mut self, text: StrTendril) -> Result<(), parser::Error> {
        for ret in try!(parser::parse_content(text, self.sink.fragments())) {
            self.sink.process_token(match ret {
                ReturnType::String(text) => Token::Text(Content::String(text)),
                ReturnType::Placeholder(path, ty) => Token::Text(Content::Placeholder(path, ty)),
                ReturnType::Logic(_) => return Err("logic can not be used as text".into()),
                ReturnType::Scope(scope) => Token::Scope(scope),
                ReturnType::End => Token::End,
            });
        }

        Ok(())
    }

    fn open_tag(&mut self, name: Atom, attributes: Vec<Attribute>, self_closing: bool) -> Result<(), parser::Error> {
        let void = is_void(&name);
        self.sink.process_token(Token::BeginTag(name));

        for attribute in attributes {
            let mut content = try!(parser::parse_content(attribute.value, self.sink.fragments())).into_iter();
            match content.next() {
                Some(ReturnType::String(text)) => self.sink.process_token(Token::BeginAttribute(attribute.name.local, Content::String(text))),
                Some(ReturnType::Placeholder(name, ty)) => {
                    self.sink.process_token(Token::BeginAttribute(attribute.name.local, Content::Placeholder(name, ty)));
                },
                Some(ReturnType::Logic(_)) => return Err("logic can not be used as text".into()),
                Some(ReturnType::Scope(scope)) => {
                    self.sink.process_token(Token::BeginAttribute(attribute.name.local, Content::String(StrTendril::new())));
                    self.sink.process_token(Token::Scope(scope));
                },
                Some(ReturnType::End) => {
                    self.sink.process_token(Token::End);
                    self.sink.process_token(Token::BeginAttribute(attribute.name.local, Content::String(StrTendril::new())));
                },
                None => self.sink.process_token(Token::BeginAttribute(attribute.name.local, Content::String(StrTendril::new())))
            }

            for ret in content {
                match ret {
                    ReturnType::String(text) => self.sink.process_token(Token::AppendToAttribute(Content::String(text))),
                    ReturnType::Placeholder(name, ty) => self.sink.process_token(Token::AppendToAttribute(Content::Placeholder(name, ty))),
                    ReturnType::Logic(_) => return Err("logic can not be used as text".into()),
                    ReturnType::Scope(scope) => self.sink.process_token(Token::Scope(scope)),
                    ReturnType::End => self.sink.process_token(Token::End),
                }
            }

            self.sink.process_token(Token::EndAttribute);
        }

        self.sink.process_token(Token::EndTag(void || self_closing));

        Ok(())
    }

    fn close_tag(&mut self, name: Atom) {
        self.sink.process_token(Token::CloseTag(name));
    }
}

impl<'a, T: TokenSink> html5ever::tokenizer::TokenSink for &'a mut Tokenizer<T> {
    fn process_token(&mut self, token: HtmlToken) {
        match token {
            HtmlToken::TagToken(Tag {
                kind: TagKind::StartTag,
                name,
                attrs,
                self_closing
            }) => if let Err(e) = self.open_tag(name, attrs, self_closing) {
                self.errors.push(e.into());
            },
            HtmlToken::TagToken(Tag {
                kind: TagKind::EndTag,
                name,
                ..
            }) => self.close_tag(name),
            HtmlToken::CharacterTokens(text) => if let Err(e) = self.add_text(text) {
                self.errors.push(e.into());
            },
            HtmlToken::DoctypeToken(doctype) => self.set_doctype(doctype),

            HtmlToken::CommentToken(_) => {},
            HtmlToken::NullCharacterToken => {},
            HtmlToken::EOFToken => {},

            HtmlToken::ParseError(e) => self.errors.push(e.into())
        }
    }
}

pub trait TokenSink {
    fn process_token(&mut self, token: Token);
    fn fragments(&self) -> &ExtensibleMap<&'static str, Box<Fragment>>;
}

// http://www.w3.org/TR/html5/syntax.html#void-elements
fn is_void(tag: &str) -> bool {
    match tag {
        "area" |
        "base" |
        "br" |
        "col" |
        "embed" |
        "hr" |
        "img" |
        "input" |
        "keygen" |
        "link" |
        "meta" |
        "param" |
        "source" |
        "track" |
        "wbr" => true,
        _ => false
    }
}
