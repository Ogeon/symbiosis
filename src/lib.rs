extern crate html5ever;
extern crate string_cache;
extern crate tendril;

use std::path::Path;
use std::fs::{File, read_dir};
use std::io::{self, Read, Write};
use std::default::Default;
use std::borrow::Cow;
use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::fmt;
use std::mem::swap;

use html5ever::Attribute;
use html5ever::tokenizer::{Tokenizer, TokenSink, Tag, TagKind, Doctype};
use html5ever::tokenizer::Token as HtmlToken;

use string_cache::atom::Atom;

use tendril::StrTendril;

use codegen::{Token, Content, Codegen, ContentType, Scope};
use fragments::{Fragment, ReturnType};

use parser::ExtensibleMap;

#[macro_use]
pub mod codegen;
pub mod fragments;
pub mod javascript;
pub mod rust;
mod parser;

#[derive(Debug)]
pub enum Error {
    Io(io::Error),
    Format(fmt::Error),
    Parse(Vec<Cow<'static, str>>)
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
                try!(write!(f, "parse errors: "));
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
    templates: Vec<ParsedTemplate>,
    fragments: HashMap<&'static str, Box<Fragment + 'a>>
}

impl<'a> TemplateGroup<'a> {
    pub fn new() -> TemplateGroup<'a> {
        TemplateGroup {
            templates: vec![],
            fragments: init_fragments()
        }
    }

    ///Add a template from a string.
    pub fn parse_string(&mut self, name: String, source: String) -> Result<(), Error> {
        let mut template = Template::extending(&self.fragments);
        try!(template.parse_string(source));
        self.templates.push(ParsedTemplate {
            name: name,
            parameters: template.parameters,
            tokens: template.tokens
        });

        Ok(())
    }

    ///Add every `*.html` and `*.htm` file in a directory to the template group.
    pub fn parse_directory<P: AsRef<Path>>(&mut self, dir: P) -> Result<(), Error> {
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

    ///Register a custom fragment.
    pub fn register_fragment<F: Fragment + 'a>(&mut self, fragment: F) {
        let fragment = Box::new(fragment);
        self.fragments.insert(fragment.identifier(), fragment);
    }

    ///Write template code.
    pub fn emit_code<W: Write, C: Codegen>(&self, writer: &mut W, codegen: &C) -> Result<(), C::Error> {
        let mut writer = codegen.init_writer(writer);
        codegen.build_module(&mut writer, |w| {
            for template in &self.templates {
                try!(codegen.build_template(w, &template.name, &template.parameters, &template.tokens));
            }
            Ok(())
        })
    }
}

struct ParsedTemplate {
    name: String,
    parameters: HashMap<StrTendril, ContentType>,
    tokens: Vec<codegen::Token>
}

///A single template.
pub struct Template<'a> {
    fragments: ExtensibleMap<'a, &'static str, Box<Fragment + 'a>>,
    parameters: HashMap<StrTendril, ContentType>,
    tokens: Vec<codegen::Token>,
    scopes: Vec<Option<(StrTendril, StrTendril, Option<ContentType>, Option<StrTendril>)>>,
    errors: Vec<Cow<'static, str>>
}

impl<'a> Template<'a> {
    ///Create an empty template.
    pub fn new() -> Template<'a> {
        Template {
            fragments: ExtensibleMap::Owned(init_fragments()),
            parameters: HashMap::new(),
            tokens: vec![],
            scopes: vec![],
            errors: vec![]
        }
    }

    fn extending(fragments: &'a HashMap<&'static str, Box<Fragment + 'a>>) -> Template<'a> {
        Template {
            fragments: ExtensibleMap::extend(fragments),
            parameters: HashMap::new(),
            tokens: vec![],
            scopes: vec![],
            errors: vec![]
        }
    }

    ///Add content from a string to the template.
    pub fn parse_string(&mut self, source: String) -> Result<(), Error> {
        {
            let mut tokenizer = Tokenizer::new(&mut*self, Default::default());
            tokenizer.feed(source.into());
        }

        if self.errors.len() > 0 {
            let mut e = vec![];
            swap(&mut e, &mut self.errors);
            Err(Error::Parse(e))
        } else {
            Ok(())
        }
    }

    ///Register a custom fragment.
    pub fn register_fragment<F: Fragment + 'a>(&mut self, fragment: F) {
        let fragment = Box::new(fragment);
        self.fragments.insert(fragment.identifier(), fragment);
    }

    ///Write template code.
    pub fn emit_code<W: Write, C: Codegen>(&self, template_name: &str, writer: &mut W, codegen: &C) -> Result<(), C::Error> {
        let mut writer = codegen.init_writer(writer);
        codegen.build_template(&mut writer, template_name, &self.parameters, &self.tokens)
    }

    fn set_doctype(&mut self, doctype: Doctype) {
        self.tokens.push(Token::SetDoctype(doctype));
    }

    fn open_tag(&mut self, name: Atom, attributes: Vec<Attribute>, self_closing: bool) -> Result<(), Cow<'static, str>> {
        let void = is_void(name.as_slice());
        self.tokens.push(Token::BeginTag(name));

        for attribute in attributes {
            let mut content = try!(parser::parse_content(attribute.value, &self.fragments)).into_iter();
            match content.next() {
                Some(ReturnType::String(text)) => self.tokens.push(Token::BeginAttribute(attribute.name.local, Content::String(text))),
                Some(ReturnType::Placeholder(name, ty)) => {
                    try!(self.reg_placeholder(name.clone(), ty));
                    self.tokens.push(Token::BeginAttribute(attribute.name.local, Content::Placeholder(name)));
                },
                Some(ReturnType::Logic(_)) => return Err(Cow::Borrowed("logic can not be used as text")),
                Some(ReturnType::Scope(scope)) => {
                    self.tokens.push(Token::BeginAttribute(attribute.name.local, Content::String(StrTendril::new())));
                    try!(self.reg_scope_vars(&scope));
                    self.tokens.push(Token::Scope(scope));
                },
                Some(ReturnType::End) => {
                    try!(self.end_scope());
                    self.tokens.push(Token::End);
                    self.tokens.push(Token::BeginAttribute(attribute.name.local, Content::String(StrTendril::new())));
                },
                None => self.tokens.push(Token::BeginAttribute(attribute.name.local, Content::String(StrTendril::new())))
            }

            for part in content {
                match part {
                    ReturnType::String(text) => self.tokens.push(Token::AppendToAttribute(Content::String(text))),
                    ReturnType::Placeholder(name, ty) => {
                        try!(self.reg_placeholder(name.clone(), ty));
                        self.tokens.push(Token::AppendToAttribute(Content::Placeholder(name)));
                    },
                    ReturnType::Logic(_) => return Err(Cow::Borrowed("logic can not be used as text")),
                    ReturnType::Scope(scope) => {
                        try!(self.reg_scope_vars(&scope));
                        self.tokens.push(Token::Scope(scope));
                    },
                    ReturnType::End => {
                        try!(self.end_scope());
                        self.tokens.push(Token::End);
                    },
                }
            }

            self.tokens.push(Token::EndAttribute);
        }

        self.tokens.push(Token::EndTag(void || self_closing));

        Ok(())
    }

    fn add_text(&mut self, text: StrTendril) -> Result<(), Cow<'static, str>> {
        let mut content = try!(parser::parse_content(text, &self.fragments)).into_iter();
        match content.next() {
            Some(ReturnType::String(text)) => self.tokens.push(Token::Text(Content::String(text))),
            Some(ReturnType::Placeholder(name, ty)) => {
                try!(self.reg_placeholder(name.clone(), ty));
                self.tokens.push(Token::Text(Content::Placeholder(name)));
            },
            Some(ReturnType::Logic(_)) => return Err(Cow::Borrowed("logic can not be used as text")),
            Some(ReturnType::Scope(scope)) => {
                try!(self.reg_scope_vars(&scope));
                self.tokens.push(Token::Scope(scope));
            },
            Some(ReturnType::End) => {
                try!(self.end_scope());
                self.tokens.push(Token::End);
            },
            None => return Ok(())
        }

        for part in content {
            match part {
                ReturnType::String(text) => self.tokens.push(Token::Text(Content::String(text))),
                ReturnType::Placeholder(name, ty) => {
                    try!(self.reg_placeholder(name.clone(), ty));
                    self.tokens.push(Token::Text(Content::Placeholder(name)));
                },
                ReturnType::Logic(_) => return Err(Cow::Borrowed("logic can not be used as text")),
                ReturnType::Scope(scope) => {
                    try!(self.reg_scope_vars(&scope));
                    self.tokens.push(Token::Scope(scope));
                },
                ReturnType::End => {
                    try!(self.end_scope());
                    self.tokens.push(Token::End);
                }
            }
        }

        Ok(())
    }

    fn close_tag(&mut self, tag: Atom) {
        self.tokens.push(Token::CloseTag(tag));
    }

    fn reg_scope_vars(&mut self, scope: &Scope) -> Result<(), Cow<'static, str>> {
        match scope {
            &Scope::If(ref logic) => for p in logic.placeholders() {
                try!(self.reg_placeholder(p.clone(), ContentType::Bool));
                self.scopes.push(None);
            },
            &Scope::ForEach(ref collection, ref element, ref opt_key) => {
                try!(self.reg_placeholder(collection.clone(), ContentType::Collection(None, false)));
                self.scopes.push(Some((collection.clone(), element.clone(), None, opt_key.clone())));
            }
        }

        Ok(())
    }

    fn reg_placeholder(&mut self, param: StrTendril, pref_ty: ContentType) -> Result<(), Cow<'static, str>> {
        println!("registering placeholder '{}', with {} scopes", param, self.scopes.len());
        for scope in self.scopes.iter_mut().rev() {
            if let &mut Some((ref _origin, ref element, ref mut ty, ref opt_key)) = scope {
                if element == &param {
                    match &mut *ty {
                        &mut Some(ref mut ty) => try!(ty.combine_with(pref_ty)),
                        ty => *ty = Some(pref_ty)
                    }

                    println!("type of '{}' seems to be {:?}", element, ty);
                    return Ok(())
                } else {
                    println!("'{}' did not match '{}'", param, element);
                }

                if let &Some(ref key) = opt_key {
                    if key == &param {
                        match pref_ty {
                            ContentType::String(false) => return Ok(()),
                            ty => return Err(Cow::Owned(format!("{} is a collection key and cannot be used as {}", param, ty)))
                        }
                    }
                }
            }
        }

        match self.parameters.entry(param) {
            Entry::Occupied(mut entry) => try!(entry.get_mut().combine_with(pref_ty)),
            Entry::Vacant(entry) => { entry.insert(pref_ty); },
        }

        Ok(())
    }

    fn end_scope(&mut self) -> Result<(), Cow<'static, str>> {
        if let Some(scope) = self.scopes.pop() {
            if let Some((origin, _, ty, _)) = scope {
                self.reg_placeholder(origin, ContentType::Collection(ty.map(|t| Box::new(t)), false))
            } else {
                Ok(())
            }
        } else {
            Err(Cow::Borrowed("unexpected end of scope"))
        }
    }
}

impl<'a, 'b: 'a> TokenSink for &'a mut Template<'b> {
    fn process_token(&mut self, token: HtmlToken) {
        match token {
            HtmlToken::TagToken(Tag {
                kind: TagKind::StartTag,
                name,
                attrs,
                self_closing
            }) => if let Err(e) = self.open_tag(name, attrs, self_closing) {
                self.errors.push(e);
            },
            HtmlToken::TagToken(Tag {
                kind: TagKind::EndTag,
                name,
                ..
            }) => self.close_tag(name),
            HtmlToken::CharacterTokens(text) => if let Err(e) = self.add_text(text) {
                self.errors.push(e);
            },
            HtmlToken::DoctypeToken(doctype) => self.set_doctype(doctype),

            HtmlToken::CommentToken(_) => {},
            HtmlToken::NullCharacterToken => {},
            HtmlToken::EOFToken => {},

            HtmlToken::ParseError(e) => self.errors.push(e)
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

fn init_fragments<'a>() -> HashMap<&'static str, Box<Fragment + 'a>> {
    let mut map = HashMap::new();

    let f: Box<Fragment + 'a> = Box::new(fragments::If);
    map.insert(f.identifier(), f);

    let f = fragments::And;
    map.insert(f.identifier(), Box::new(f));

    let f = fragments::Or;
    map.insert(f.identifier(), Box::new(f));

    let f = fragments::Not;
    map.insert(f.identifier(), Box::new(f));

    let f = fragments::Template;
    map.insert(f.identifier(), Box::new(f));

    let f = fragments::ForEach;
    map.insert(f.identifier(), Box::new(f));

    map
}