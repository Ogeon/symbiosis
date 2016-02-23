extern crate html5ever;
#[macro_use]
extern crate string_cache;
extern crate symbiosis_tokenizer;

use std::path;
use std::fs::{File, read_dir};
use std::io::{Read, Write};
use std::borrow::Cow;
use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::mem::swap;

pub use symbiosis_tokenizer::{fragment, Error, StrTendril};
use symbiosis_tokenizer::{Tokenizer, TokenSink};
use symbiosis_tokenizer::parser::{self, ExtensibleMap};

use codegen::{Token, Content, Codegen, ContentType, Scope, Path, Params};
use fragment::Fragment;


#[macro_use]
pub mod codegen;
pub mod javascript;
pub mod rust;

///A collection of templates.
pub struct TemplateGroup {
    templates: Vec<ParsedTemplate>,
    fragments: HashMap<&'static str, Box<Fragment>>
}

impl TemplateGroup {
    pub fn new() -> TemplateGroup {
        TemplateGroup {
            templates: vec![],
            fragments: init_fragments()
        }
    }

    ///Add a template from a string.
    pub fn parse_string<S: Into<StrTendril>>(&mut self, name: String, source: S) -> Result<(), Error> {
        let mut template = Template::extending(&self.fragments);
        try!(template.parse_string(source.into()));
        self.templates.push(ParsedTemplate {
            name: name,
            parameters: template.parameters,
            tokens: template.tokens
        });

        Ok(())
    }

    ///Add every `*.html` and `*.htm` file in a directory to the template group.
    pub fn parse_directory<P: AsRef<path::Path>>(&mut self, dir: P) -> Result<(), Error> {
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
    pub fn register_fragment<F: Fragment + 'static>(&mut self, fragment: F) {
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
    parameters: Params,
    tokens: Vec<codegen::Token>
}

///A single template.
pub struct Template<'a> {
    fragments: ExtensibleMap<'a, &'static str, Box<Fragment>>,
    parameters: Params,
    tokens: Vec<codegen::Token>,
    scopes: Vec<Option<(Path, StrTendril, Option<ContentType>, Option<StrTendril>)>>,
    errors: Vec<parser::Error>,
}

impl<'a> Template<'a> {
    ///Create an empty template.
    pub fn new() -> Template<'a> {
        Template {
            fragments: ExtensibleMap::Owned(init_fragments()),
            parameters: Params::new(),
            tokens: vec![],
            scopes: vec![],
            errors: vec![],
        }
    }

    fn extending(fragments: &'a HashMap<&'static str, Box<Fragment>>) -> Template<'a> {
        Template {
            fragments: ExtensibleMap::extend(fragments),
            parameters: Params::new(),
            tokens: vec![],
            scopes: vec![],
            errors: vec![],
        }
    }

    ///Add content from a string to the template.
    pub fn parse_string<S: Into<StrTendril>>(&mut self, source: S) -> Result<(), Error> {
        {
            let mut tokenizer = Tokenizer::new(&mut *self);
            try!(tokenizer.parse_string(source.into()));
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
    pub fn register_fragment<F: Fragment + 'static>(&mut self, fragment: F) {
        let fragment = Box::new(fragment);
        self.fragments.insert(fragment.identifier(), fragment);
    }

    ///Write template code.
    pub fn emit_code<W: Write, C: Codegen>(&self, template_name: &str, writer: &mut W, codegen: &C) -> Result<(), C::Error> {
        let mut writer = codegen.init_writer(writer);
        codegen.build_template(&mut writer, template_name, &self.parameters, &self.tokens)
    }

    fn reg_scope_vars(&mut self, scope: &Scope) -> Result<(), Cow<'static, str>> {
        match scope {
            &Scope::If(ref logic) => for p in logic.placeholders() {
                try!(self.reg_placeholder(p.clone().into(), ContentType::Bool));
                self.scopes.push(None);
            },
            &Scope::ForEach(ref collection, ref element, ref opt_key) => {
                try!(self.reg_placeholder(collection.clone(), ContentType::Collection(None, false)));
                self.scopes.push(Some((collection.clone(), element.clone(), None, opt_key.clone())));
            }
        }

        Ok(())
    }

    fn reg_placeholder(&mut self, path: Path, pref_ty: ContentType) -> Result<(), Cow<'static, str>> {
        let mut path = path.into_iter();
        let param = path.next().expect("found an empty path");
        let mut path = path.rev();

        let pref_ty = if let Some(last) = path.next() {
            let mut fields = Params::new();
            fields.insert(last.into(), pref_ty);
            for part in path {
                let next = ::std::mem::replace(&mut fields, Params::new());
                fields.insert(part.into(), ContentType::Struct(None, next, false));
            }

            ContentType::Struct(None, fields, false)
        } else {
            pref_ty
        };


        for scope in self.scopes.iter_mut().rev() {
            if let &mut Some((ref _origin, ref element, ref mut ty, ref opt_key)) = scope {
                if element == &param {
                    match &mut *ty {
                        &mut Some(ref mut ty) => try!(ty.combine_with(pref_ty)),
                        ty => *ty = Some(pref_ty)
                    }
                    return Ok(())
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
                self.reg_placeholder(origin.into(), ContentType::Collection(ty.map(|t| Box::new(t)), false))
            } else {
                Ok(())
            }
        } else {
            Err(Cow::Borrowed("unexpected end of scope"))
        }
    }
}

impl<'a, 'b: 'a> TokenSink for &'a mut Template<'b> {
    fn process_token(&mut self, token: Token) {
        match token {
            Token::SetDoctype(doctype) => self.tokens.push(Token::SetDoctype(doctype)),
            Token::Comment(comment) => self.tokens.push(Token::Comment(comment)),
            Token::BeginTag(name) => self.tokens.push(Token::BeginTag(name)),
            Token::EndTag(self_close) => self.tokens.push(Token::EndTag(self_close)),
            Token::CloseTag(name) => self.tokens.push(Token::CloseTag(name)),

            Token::BeginAttribute(attr, content) => match content {
                Content::String(content) => self.tokens.push(Token::BeginAttribute(attr, Content::String(content))),
                Content::Placeholder(name, ty) => {
                    if let Err(e) = self.reg_placeholder(name.clone(), ty.clone()) {
                        self.errors.push(e.into());
                    }
                    self.tokens.push(Token::BeginAttribute(attr, Content::Placeholder(name, ty)));
                }
            },
            Token::AppendToAttribute(content) => match content {
                Content::String(content) => self.tokens.push(Token::AppendToAttribute(Content::String(content))),
                Content::Placeholder(name, ty) => {
                    if let Err(e) = self.reg_placeholder(name.clone(), ty.clone()) {
                        self.errors.push(e.into());
                    }
                    self.tokens.push(Token::AppendToAttribute(Content::Placeholder(name, ty)));
                }
            },
            Token::Text(content) => match content {
                Content::String(content) => self.tokens.push(Token::Text(Content::String(content))),
                Content::Placeholder(name, ty) => {
                    if let Err(e) = self.reg_placeholder(name.clone(), ty.clone()) {
                        self.errors.push(e.into());
                    }
                    self.tokens.push(Token::Text(Content::Placeholder(name, ty)));
                }
            },
            Token::EndAttribute => self.tokens.push(Token::EndAttribute),
            Token::Scope(scope) => {
                if let Err(e) = self.reg_scope_vars(&scope) {
                    self.errors.push(e.into());
                }
                self.tokens.push(Token::Scope(scope));
            },
            Token::End => {
                if let Err(e) = self.end_scope() {
                    self.errors.push(e.into());
                }
                self.tokens.push(Token::End);
            },
        }
    }

    fn fragments(&self) -> &ExtensibleMap<&'static str, Box<Fragment>> {
        &self.fragments
    }
}

fn init_fragments<'a>() -> HashMap<&'static str, Box<Fragment + 'a>> {
    let mut map = HashMap::new();

    let f: Box<Fragment + 'a> = Box::new(fragment::If);
    map.insert(f.identifier(), f);

    let f = fragment::And;
    map.insert(f.identifier(), Box::new(f));

    let f = fragment::Or;
    map.insert(f.identifier(), Box::new(f));

    let f = fragment::Not;
    map.insert(f.identifier(), Box::new(f));

    let f = fragment::ForEach;
    map.insert(f.identifier(), Box::new(f));

    map
}