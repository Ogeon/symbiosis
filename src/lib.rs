#![feature(std_misc)]

extern crate html5ever;
extern crate string_cache;

use std::path::AsPath;
use std::fs::{File, read_dir};
use std::io::{self, Read, Write};
use std::error::FromError;
use std::default::Default;
use std::borrow::Cow;
use std::collections::HashSet;
use std::fmt;

use html5ever::Attribute;
use html5ever::tokenizer::{Tokenizer, TokenSink, Tag, TagKind};
use html5ever::tokenizer::Token as HtmlToken;

use string_cache::atom::Atom;

pub use codegen::{JavaScript, JsVarState, Rust, RustVisibility};

use codegen::{Token, Content, Codegen};

use parser::Token as ParserToken;

pub mod codegen;
mod parser;

#[derive(Debug)]
pub enum Error {
    Io(io::Error),
    Parse(Vec<Cow<'static, str>>)
}

impl FromError<io::Error> for Error {
    fn from_error(e: io::Error) -> Error {
        Error::Io(e)
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Error::Io(ref e) => e.fmt(f),
            &Error::Parse(ref e) => {
                try!(write!(f, "Parse errors: "));
                for (i, error) in e.iter().enumerate() {
                    if i == 0 {
                        try!(error.fmt(f));
                    } else {
                        try!(write!(f, ", {}", error));
                    }
                }
                Ok(())
            }
        }
    }
}

///A collection of templates.
pub struct TemplateGroup {
    templates: Vec<ParsedTemplate>
}

impl TemplateGroup {
    pub fn new() -> TemplateGroup {
        TemplateGroup {
            templates: vec![]
        }
    }

    ///Add a template from a string.
    pub fn parse_string(&mut self, name: String, source: String) -> Result<(), Error> {
        let mut template = Template::new();
        try!(template.parse_string(source));
        self.templates.push(ParsedTemplate {
            name: name,
            parameters: template.parameters,
            tokens: template.tokens
        });

        Ok(())
    }

    ///Add every `*.html` and `*.htm` file in a directory to the template group.
    pub fn parse_directory<P: AsPath>(&mut self, dir: P) -> Result<(), Error> {
        let directory = try!(read_dir(dir));

        for path in directory {
            let path = try!(path).path();
            let info = (
                path.file_stem().and_then(|n| n.to_str()),
                path.extension().and_then(|e| e.to_str())
            );
            if let (Some(name), Some(ext)) = info {
                if ext == "html" || ext == "htm" {
                    let mut source = String::new();
                    try!(File::open(&path).and_then(|mut f| f.read_to_string(&mut source)));
                    try!(self.parse_string(name.to_string(), source));
                }
            }
        }

        Ok(())
    }

    ///Add an already parsed template to the group.
    pub fn add_template(&mut self, name: String, template: Template) {
        self.templates.push(ParsedTemplate {
            name: name,
            parameters: template.parameters,
            tokens: template.tokens
        });
    }

    ///Write template code.
    pub fn emit_code<W: Write, C: Codegen>(&self, writer: &mut W, codegen: &C) -> io::Result<()> {
        codegen.build_module(writer, |w, indent| {
            for template in &self.templates {
                try!(codegen.build_template(w, &template.name, indent, &template.parameters, &template.tokens));
            }
            Ok(())
        })
    }
}

struct ParsedTemplate {
    name: String,
    parameters: HashSet<String>,
    tokens: Vec<codegen::Token>
}

///A single template.
pub struct Template {
    parameters: HashSet<String>,
    tokens: Vec<codegen::Token>
}

impl Template {
    ///Create an empty template.
    pub fn new() -> Template {
        Template {
            parameters: HashSet::new(),
            tokens: vec![]
        }
    }

    ///Add content from a string to the template.
    pub fn parse_string(&mut self, source: String) -> Result<(), Error> {
        let mut tokenizer = Tokenizer::new(self, Default::default());
        tokenizer.feed(source);

        Ok(())
    }

    ///Write template code.
    pub fn emit_code<W: Write, C: Codegen>(&self, template_name: &str, writer: &mut W, codegen: &C) -> io::Result<()> {
        codegen.build_template(writer, template_name, 0, &self.parameters, &self.tokens)
    }

    fn open_tag(&mut self, name: Atom, attributes: Vec<Attribute>, self_closing: bool) {
        let void = is_void(name.as_slice());
        self.tokens.push(Token::BeginTag(name));

        for attribute in attributes {
            let mut content = parser::parse_content(&attribute.value).into_iter();
            match content.next() {
                Some(ParserToken::Text(text)) => self.tokens.push(Token::BeginAttribute(attribute.name.local, Content::String(text))),
                Some(ParserToken::Placeholder(name)) => {
                    self.parameters.insert(name.clone());
                    self.tokens.push(Token::BeginAttribute(attribute.name.local, Content::Placeholder(name)));
                },
                None => self.tokens.push(Token::BeginAttribute(attribute.name.local, Content::String(String::new())))
            }

            for part in content {
                match part {
                    ParserToken::Text(text) => self.tokens.push(Token::AppendToAttribute(Content::String(text))),
                    ParserToken::Placeholder(name) => {
                        self.parameters.insert(name.clone());
                        self.tokens.push(Token::AppendToAttribute(Content::Placeholder(name)));
                    }
                }
            }

            self.tokens.push(Token::EndAttribute);
        }

        self.tokens.push(Token::EndTag(void || self_closing));
    }

    fn add_text(&mut self, text: String) {
        let mut content = parser::parse_content(&text).into_iter();
        match content.next() {
            Some(ParserToken::Text(text)) => self.tokens.push(Token::BeginText(Content::String(text))),
            Some(ParserToken::Placeholder(name)) => {
                self.parameters.insert(name.clone());
                self.tokens.push(Token::BeginText(Content::Placeholder(name)));
            },
            None => return
        }

        for part in content {
            match part {
                ParserToken::Text(text) => self.tokens.push(Token::AppendToText(Content::String(text))),
                ParserToken::Placeholder(name) => {
                    self.parameters.insert(name.clone());
                    self.tokens.push(Token::AppendToText(Content::Placeholder(name)));
                }
            }
        }
        self.tokens.push(Token::EndText);
    }

    fn close_tag(&mut self, tag: Atom) {
        self.tokens.push(Token::CloseTag(tag));
    }
}

impl<'a> TokenSink for &'a mut Template {
    fn process_token(&mut self, token: HtmlToken) {
        match token {
            HtmlToken::TagToken(Tag {
                kind: TagKind::StartTag,
                name,
                attrs,
                self_closing
            }) => self.open_tag(name, attrs, self_closing),
            HtmlToken::TagToken(Tag {
                kind: TagKind::EndTag,
                name,
                ..
            }) => self.close_tag(name),
            HtmlToken::CharacterTokens(text) => self.add_text(text),
            _ => {}
        }
    }
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