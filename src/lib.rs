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

use codegen::Token;

use parser::Token as ParserToken;

mod codegen;
mod parser;

pub fn parse_directory<'a, P: AsPath>(dir: P) -> Result<TemplateGroup<'a>, Error> {
    let directories = try!(read_dir(dir));
    let mut parser = TemplateGroup::new();

    for directory in directories {
        let directory = try!(directory).path();
        if let Some(name) = directory.file_stem() {
            if let Some(name) = name.to_str() {
                let mut source = String::new();
                try!(File::open(&directory).and_then(|mut f| f.read_to_string(&mut source)));
                try!(parser.add_template(name.to_string(), source));
            }
        }
    }

    Ok(parser)
}

pub fn parse_string(source: String) -> Result<Template, Error> {
    let mut tokenizer = Tokenizer::new(Template::new(), Default::default());
    tokenizer.feed(source);
    Ok(tokenizer.unwrap())
}

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
pub struct TemplateGroup<'a> {
    templates: Vec<(String, Template)>,
    namespace: Option<&'a str>,
    ns_predefined: bool
}

impl<'a> TemplateGroup<'a> {
    pub fn new() -> TemplateGroup<'a> {
        TemplateGroup {
            templates: vec![],
            namespace: None,
            ns_predefined: false
        }
    }

    ///Set the name of the namespace where the Javascript templates should be
    ///defined. Set `predefined` to true if it was defined somewhere else.
    pub fn js_namespace(&mut self, namespace: &'a str, predefined: bool) {
        self.namespace = Some(namespace);
        self.ns_predefined = predefined;
    }

    pub fn add_template(&mut self, name: String, source: String) -> Result<(), Error> {
        let template = try!(parse_string(source));
        self.templates.push((name, template));
        Ok(())
    }

    pub fn emit_rust<W: Write>(&self, writer: &mut W) -> io::Result<()> {
        for &(ref name, ref template) in &self.templates {
            try!(template.emit_rust(true, name, writer));
        }
        Ok(())
    }

    pub fn emit_js<W: Write>(&self, writer: &mut W) -> io::Result<()> {
        if let Some(ref namespace) = self.namespace {
            if !self.ns_predefined {
                try!(write!(writer, "var {} = {{}};\n", namespace));
            }
        }

        for &(ref name, ref template) in &self.templates {
            if let Some(namespace) = self.namespace {
                try!(template.emit_js(false, &format!("{}.{}", namespace, name), writer));
            } else {
                try!(template.emit_js(true, name, writer));
            }
        }
        Ok(())
    }
}

pub struct Template {
    parameters: HashSet<String>,
    tokens: Vec<codegen::Token>
}

impl Template {
    fn new() -> Template {
        Template {
            parameters: HashSet::new(),
            tokens: vec![]
        }
    }

    fn open_tag(&mut self, name: Atom, attributes: Vec<Attribute>, self_closing: bool) {
        let void = is_void(name.as_slice());
        self.tokens.push(Token::BeginTag(name));

        for attribute in attributes {
            let mut content = parser::parse_content(&attribute.value).into_iter();
            match content.next() {
                Some(ParserToken::Text(text)) => self.tokens.push(Token::BeginAttribute(attribute.name.local, text)),
                None => self.tokens.push(Token::BeginAttribute(attribute.name.local, "".to_string()))
            }

            for part in content {
                match part {
                    ParserToken::Text(text) => self.tokens.push(Token::AppendToAttribute(text)),
                }
            }

            self.tokens.push(Token::EndAttribute);
        }

        self.tokens.push(Token::EndTag(void || self_closing));
    }

    fn add_text(&mut self, text: String) {
        for part in parser::parse_content(&text) {
            match part {
                ParserToken::Text(text) => self.tokens.push(Token::AddText(text)),
            }
        }
    }

    fn close_tag(&mut self, tag: Atom) {
        self.tokens.push(Token::CloseTag(tag));
    }

    ///Write a Rust template and define it as `name`. The template will be
    ///prefixed with `pub` if `public` is true.
    pub fn emit_rust<W: Write>(&self, public: bool, name: &str, writer: &mut W) -> io::Result<()> {
        if public {
            try!(write!(writer, "pub "));
        }

        let lifetime = if self.parameters.len() > 0 {
            try!(write!(writer, "struct {}<'a> {{\n", name));

            for parameter in &self.parameters {
                try!(write!(writer, "    pub {}: Option<&'a str>,\n", parameter));
            }

            try!(write!(writer, "}}\n\n"));
            Some("'a")
        } else {
            try!(write!(writer, "struct {};\n\n", name));
            None
        };

        codegen::build_rust(writer, name, lifetime, &self.tokens)
    }

    ///Write a Javascript template and define it as `name`. The template will
    ///be prefixed with `var` if `local` is true.
    pub fn emit_js<W: Write>(&self, local: bool, name: &str, writer: &mut W) -> io::Result<()> {
        if local {
            try!(write!(writer, "var "));
        }

        try!(write!(writer, "{} = function() {{\n", name));

        for parameter in &self.parameters {
            try!(write!(writer, "    this.{} = \"\";\n", parameter));
        }

        try!(write!(writer, "}};\n\n"));
        codegen::build_js(writer, name, &self.tokens)
    }
}

impl TokenSink for Template {
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