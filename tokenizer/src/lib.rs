extern crate html5ever;
extern crate string_cache;
extern crate symbiosis_core;

use std::mem::replace;
use std::fmt;
use std::io;

pub use symbiosis_core::StrTendril;

use html5ever::Attribute;
use html5ever::tokenizer::{Tag, TagKind, Doctype};
use html5ever::tokenizer::Token as HtmlToken;
use html5ever::tokenizer::states::{State, ScriptData, Rcdata, Rawtext};

use string_cache::Atom;

use codegen::{Token, Content, Name, Text};
use fragment::{ReturnType, FragmentStore};

use fragment::Error as ParseError;

//haxx
mod symbiosis_tokenizer {
    pub use super::*;
}
mod parser;

pub mod codegen;
pub mod fragment;

#[derive(Debug)]
pub enum Error {
    Io(io::Error),
    Format(fmt::Error),
    Parse(Vec<ParseError>),
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

impl From<ParseError> for Error {
    fn from(e: ParseError) -> Error {
        Error::Parse(vec![e])
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
    errors: Vec<ParseError>,
    parser_state: Option<State>,
    escape: bool,
}

impl<T: TokenSink> Tokenizer<T> {
    pub fn new(sink: T) -> Tokenizer<T> {
        Tokenizer {
            sink: sink,
            errors: vec![],
            parser_state: None,
            escape: true,
        }
    }

    pub fn parse_string(&mut self, source: StrTendril) -> Result<(), Error> {
        {
            let mut tokenizer = html5ever::tokenizer::Tokenizer::new(&mut*self, Default::default());
            tokenizer.feed(source);
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

    fn add_comment(&mut self, comment: StrTendril) {
        self.sink.process_token(Token::Comment(comment));
    }

    fn add_text(&mut self, text: StrTendril) -> Result<(), ParseError> {
        for ret in try!(parser::parse_content(text, self.sink.fragments())) {
            match ret {
                ReturnType::String(text) => self.sink.process_token(Token::Text(Content::String(Text::new(text, self.escape, false)))),
                ReturnType::Placeholder(path, ty) => self.sink.process_token(Token::Text(Content::Placeholder(path, ty))),
                ReturnType::Logic(_) => return Err("logic can not be used as text".into()),
                ReturnType::Scope(scope) => self.sink.process_token(Token::Scope(scope)),
                ReturnType::End => self.sink.process_token(Token::End),
                ReturnType::Tag { name, arguments, content } => try!(self.add_tag_tree(name, arguments, content)),
                ReturnType::TypeHint(path, ty) => self.sink.process_token(Token::TypeHint(path, ty)),
            };
        }

        Ok(())
    }

    fn open_tag(&mut self, name: Atom, attributes: Vec<Attribute>, self_closing: bool) -> Result<(), ParseError> {
        self.sink.process_token(Token::BeginTag(name.clone().into()));

        for attribute in attributes {
            let mut content = try!(parser::parse_content(attribute.value, self.sink.fragments())).into_iter();
            match content.next() {
                Some(ReturnType::String(text)) => self.sink.process_token(Token::BeginAttribute(attribute.name.local.into(), Content::String(Text::escaped(text, true)))),
                Some(ReturnType::Placeholder(name, ty)) => {
                    self.sink.process_token(Token::BeginAttribute(attribute.name.local.into(), Content::Placeholder(name, ty)));
                },
                Some(ReturnType::Logic(_)) => return Err("logic can not be used as text".into()),
                Some(ReturnType::Scope(scope)) => {
                    self.sink.process_token(Token::BeginAttribute(attribute.name.local.into(), Content::String(Text::empty())));
                    self.sink.process_token(Token::Scope(scope));
                },
                Some(ReturnType::End) => {
                    self.sink.process_token(Token::End);
                    self.sink.process_token(Token::BeginAttribute(attribute.name.local.into(), Content::String(Text::empty())));
                },
                Some(ReturnType::Tag { .. }) => return Err("HTML tags can not be used as pure text".into()),
                Some(ReturnType::TypeHint(path, ty)) => self.sink.process_token(Token::TypeHint(path, ty)),
                None => self.sink.process_token(Token::BeginAttribute(attribute.name.local.into(), Content::String(Text::empty())))
            }

            for ret in content {
                match ret {
                    ReturnType::String(text) => self.sink.process_token(Token::AppendToAttribute(Content::String(Text::escaped(text, true)))),
                    ReturnType::Placeholder(name, ty) => self.sink.process_token(Token::AppendToAttribute(Content::Placeholder(name, ty))),
                    ReturnType::Logic(_) => return Err("logic can not be used as text".into()),
                    ReturnType::Scope(scope) => self.sink.process_token(Token::Scope(scope)),
                    ReturnType::End => self.sink.process_token(Token::End),
                    ReturnType::Tag { .. } => return Err("HTML tags can not be used as pure text".into()),
                    ReturnType::TypeHint(path, ty) => self.sink.process_token(Token::TypeHint(path, ty)),
                }
            }

            self.sink.process_token(Token::EndAttribute);
        }

        self.sink.process_token(Token::EndTag(self_closing));

        if !self_closing {
            self.parser_state = match &*name {
                "script" => Some(State::RawData(ScriptData)),
                "title" | "style" | "plaintext" | "listing" | "pre" | "xmp" | "iframe" | "noembed" | "noframes" => Some(State::RawData(Rawtext)),
                "textarea" => Some(State::RawData(Rcdata)),
                _ => None,
            };

            self.escape = match &*name {
                "style" | "script" | "xmp" | "iframe" | "noembed" | "noframes" | "plaintext" => false,
                _ => true
            };
        }

        Ok(())
    }

    fn close_tag(&mut self, name: Atom) {
        self.sink.process_token(Token::CloseTag(name.into()));
        self.escape = true;
    }

    fn add_tag_tree(&mut self, name: Name, arguments: Vec<(Name, Vec<ReturnType>)>, content: Option<Vec<ReturnType>>) -> Result<(), ParseError> {
        self.sink.process_token(Token::BeginTag(name.clone()));

        for (attr, content) in arguments {
            let mut content = content.into_iter();
            match content.next() {
                Some(ReturnType::String(text)) => self.sink.process_token(Token::BeginAttribute(attr, Content::String(Text::escaped(text, true)))),
                Some(ReturnType::Placeholder(name, ty)) => {
                    self.sink.process_token(Token::BeginAttribute(attr, Content::Placeholder(name, ty)));
                },
                Some(ReturnType::Logic(_)) => return Err("logic can not be used as text".into()),
                Some(ReturnType::Scope(scope)) => {
                    self.sink.process_token(Token::BeginAttribute(attr, Content::String(Text::empty())));
                    self.sink.process_token(Token::Scope(scope));
                },
                Some(ReturnType::End) => {
                    self.sink.process_token(Token::End);
                    self.sink.process_token(Token::BeginAttribute(attr, Content::String(Text::empty())));
                },
                Some(ReturnType::Tag { .. }) => return Err("HTML tags can not be used as pure text".into()),
                Some(ReturnType::TypeHint(path, ty)) => self.sink.process_token(Token::TypeHint(path, ty)),
                None => self.sink.process_token(Token::BeginAttribute(attr, Content::String(Text::empty())))
            }

            for ret in content {
                match ret {
                    ReturnType::String(text) => self.sink.process_token(Token::AppendToAttribute(Content::String(Text::escaped(text, true)))),
                    ReturnType::Placeholder(name, ty) => self.sink.process_token(Token::AppendToAttribute(Content::Placeholder(name, ty))),
                    ReturnType::Logic(_) => return Err("logic can not be used as text".into()),
                    ReturnType::Scope(scope) => self.sink.process_token(Token::Scope(scope)),
                    ReturnType::End => self.sink.process_token(Token::End),
                    ReturnType::Tag { .. } => return Err("HTML tags can not be used as pure text".into()),
                    ReturnType::TypeHint(path, ty) => self.sink.process_token(Token::TypeHint(path, ty)),
                }
            }
            self.sink.process_token(Token::EndAttribute);
        }

        self.sink.process_token(Token::EndTag(content.is_none()));

        let escape = if content.is_some() {
            match &*name {
                "style" | "script" | "xmp" | "iframe" | "noembed" | "noframes" | "plaintext" => false,
                _ => true
            }
        } else {
            true
        };

        if let Some(content) = content {
            if is_void(&name) {
                return Err(format!("{} tags are not supposed to have any content", name).into());
            }

            for ret in content {
                match ret {
                    ReturnType::String(text) => self.sink.process_token(Token::Text(Content::String(Text::new(text, escape, false)))),
                    ReturnType::Placeholder(path, ty) => self.sink.process_token(Token::Text(Content::Placeholder(path, ty))),
                    ReturnType::Logic(_) => return Err("logic can not be used as text".into()),
                    ReturnType::Scope(scope) => self.sink.process_token(Token::Scope(scope)),
                    ReturnType::End => self.sink.process_token(Token::End),
                    ReturnType::Tag { name, arguments, content } => try!(self.add_tag_tree(name, arguments, content)),
                    ReturnType::TypeHint(path, ty) => self.sink.process_token(Token::TypeHint(path, ty)),
                }
            }


            self.sink.process_token(Token::CloseTag(name));
        }

        Ok(())
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

            HtmlToken::CommentToken(comment) => self.add_comment(comment),
            HtmlToken::NullCharacterToken => {},
            HtmlToken::EOFToken => {},

            HtmlToken::ParseError(e) => self.errors.push(e.into())
        }
    }

    fn query_state_change(&mut self) -> Option<State> {
        self.parser_state.take()
    }
}

pub trait TokenSink {
    fn process_token(&mut self, token: Token);
    fn fragments(&self) -> &FragmentStore;
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
