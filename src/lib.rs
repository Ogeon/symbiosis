extern crate symbiosis_tokenizer;

use std::path;
use std::fs::{File, read_dir};
use std::io::{Read, Write};
use std::borrow::{Cow, Borrow};
use std::hash::Hash;
use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::mem::swap;

pub use symbiosis_tokenizer::{fragment, Error, StrTendril};
use symbiosis_tokenizer::{Tokenizer, TokenSink};
use symbiosis_tokenizer::parser;
use symbiosis_tokenizer::codegen::{ContentType, Params};
use symbiosis_tokenizer::codegen::Token as CoreToken;
use symbiosis_tokenizer::codegen::Content as CoreContent;

use codegen::{Token, Content, Codegen, Scope, Path, Type, Struct, StructName, Name};
use fragment::{Fragment, FragmentStore};


#[macro_use]
pub mod codegen;
pub mod javascript;
pub mod rust;

///A collection of templates.
pub struct TemplateGroup {
    templates: Vec<ParsedTemplate>,
    structs: HashMap<Name, Struct>,
    fragments: HashMap<&'static str, Box<Fragment>>
}

impl TemplateGroup {
    pub fn new() -> TemplateGroup {
        let mut fragments = HashMap::new();
        fragment::init_prelude(&mut fragments);

        TemplateGroup {
            templates: vec![],
            structs: HashMap::new(),
            fragments: fragments,
        }
    }

    ///Add a template from a string.
    pub fn parse_string<S: Into<StrTendril>>(&mut self, name: String, source: S) -> Result<(), Error> {
        let (params, tokens) = {
            let mut template = Template::extending(&self.fragments);
            try!(template.parse_string(source.into()));
            (template.parameters, template.tokens)
        };
        try!(self.register_struct(StructName::Given(name.clone().into()), params));
        self.templates.push(ParsedTemplate {
            name: name,
            tokens: tokens
        });

        Ok(())
    }

    pub fn parse_file<P: AsRef<path::Path>>(&mut self, name: String, file: P) -> Result<(), Error> {
        let mut source = String::new();
        try!(File::open(&file).and_then(|mut f| f.read_to_string(&mut source)));
        self.parse_string(name, source)
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
                    try!(self.parse_file(name.into(), &path));
                }
            }
        }

        Ok(())
    }

    ///Add an already parsed template to the group.
    pub fn add_template(&mut self, name: String, template: Template) -> Result<(), Error> {
        try!(self.register_struct(StructName::Given(name.clone().into()), template.parameters));
        self.templates.push(ParsedTemplate {
            name: name,
            tokens: template.tokens
        });
        Ok(())
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
            let structs = (&self.structs).into();
            try!(codegen.build_structs(w, structs));
            for template in &self.templates {
                let params = self.structs.get(&*template.name).expect("missing struct definition for template");
                try!(codegen.build_template(w, &template.name, &params.fields, structs, &template.tokens));
            }
            Ok(())
        })
    }

    ///Get one of the generated structs.
    pub fn get_struct(&self, name: &str) -> Option<&Struct> {
        self.structs.get(name)
    }

    fn register_struct(&mut self, name: StructName, parameters: Params) -> Result<(), parser::Error> {
        let mut s = Struct {
            name: name.clone(),
            fields: HashMap::new(),
        };

        for (field_name, ty) in parameters {
            let ty = try!(self.interpret_type(ty, || {
                let mut new_name = String::from((*name).clone());
                new_name.push_str(&to_camel_case(&field_name));
                new_name.into()
            }));

            s.fields.insert(field_name.into(), ty);
        }

        let mut renames = vec![];

        match self.structs.entry(name.into()) {
            Entry::Occupied(e) => {
                let e = e.into_mut();
                if e.name.is_generated() {
                    e.name = s.name;
                }

                for (field, ty) in s.fields {
                    match e.fields.entry(field) {
                        Entry::Occupied(e) => {
                            let e = e.into_mut();
                            if let Some(rename) = try!(e.merge_with(ty)) {
                                renames.push(rename);
                            }
                        },
                        Entry::Vacant(e) => {
                            e.insert(ty);
                        }
                    }
                }
            },
            Entry::Vacant(e) => {
                e.insert(s);
            }
        }

        while let Some((old, new)) = renames.pop() {
            for (_, s) in &mut self.structs {
                for (_, ty) in &mut s.fields {
                    ty.rename_struct(&old, &new);
                }
            }

            if let Some(mut s) = self.structs.remove(&old) {
                if s.name.is_generated() {
                    s.name = StructName::Given(new.clone());
                } else {
                    return Err(format!("could not use struct '{}' instead of '{}'", new, old).into());
                }

                match self.structs.entry(new) {
                    Entry::Occupied(e) => {
                        let e = e.into_mut();
                        if e.name.is_generated() {
                            e.name = s.name;
                        }

                        for (field, ty) in s.fields {
                            match e.fields.entry(field) {
                                Entry::Occupied(e) => {
                                    let e = e.into_mut();
                                    if let Some(rename) = try!(e.merge_with(ty)) {
                                        renames.push(rename);
                                    }
                                },
                                Entry::Vacant(e) => {
                                    e.insert(ty);
                                }
                            }
                        }
                    },
                    Entry::Vacant(e) => {
                        e.insert(s);
                    }
                }
            }
        }

        Ok(())
    }

    fn interpret_type<F>(&mut self, ty: ContentType, gen_name: F) -> Result<Type, parser::Error> where
        F: FnOnce() -> Name
    {
        Ok(match ty {
            ContentType::Struct(None, fields, optional) => {
                let name = gen_name();
                try!(self.register_struct(StructName::Generated(name.clone()), fields));
                Type::Struct(name, optional)
            },
            ContentType::Struct(Some(name), fields, optional) => {
                let name = Name::from(name);
                try!(self.register_struct(StructName::Given(name.clone()), fields));
                Type::Struct(name, optional)
            },
            ContentType::String(optional) => Type::Content(optional),
            ContentType::Bool => Type::Bool,
            ContentType::Collection(Some(inner), optional) => {
                let inner = try!(self.interpret_type(*inner, gen_name));
                Type::Collection(Some(Box::new(inner)), optional)
            },
            ContentType::Collection(None, optional) => {
                Type::Collection(None, optional)
            },
        })
    }
}

struct ParsedTemplate {
    name: String,
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
        let mut fragments = HashMap::new();
        fragment::init_prelude(&mut fragments);
        Template {
            fragments: ExtensibleMap::Owned(fragments),
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

    /*///Write template code.
    pub fn emit_code<W: Write, C: Codegen>(&self, template_name: &str, writer: &mut W, codegen: &C) -> Result<(), C::Error> {
        let mut writer = codegen.init_writer(writer);
        codegen.build_template(&mut writer, template_name, &self.parameters, &self.tokens)
    }*/

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
    fn process_token(&mut self, token: CoreToken) {
        match token {
            CoreToken::SetDoctype(doctype) => self.tokens.push(Token::SetDoctype(doctype)),
            CoreToken::Comment(comment) => self.tokens.push(Token::Comment(comment)),
            CoreToken::BeginTag(name) => self.tokens.push(Token::BeginTag(name)),
            CoreToken::EndTag(self_close) => self.tokens.push(Token::EndTag(self_close)),
            CoreToken::CloseTag(name) => self.tokens.push(Token::CloseTag(name)),

            CoreToken::BeginAttribute(attr, content) => match content {
                CoreContent::String(content) => self.tokens.push(Token::BeginAttribute(attr, Content::String(content))),
                CoreContent::Placeholder(name, ty) => {
                    if let Err(e) = self.reg_placeholder(name.clone(), ty) {
                        self.errors.push(e.into());
                    }
                    self.tokens.push(Token::BeginAttribute(attr, Content::Placeholder(name)));
                }
            },
            CoreToken::AppendToAttribute(content) => match content {
                CoreContent::String(content) => self.tokens.push(Token::AppendToAttribute(Content::String(content))),
                CoreContent::Placeholder(name, ty) => {
                    if let Err(e) = self.reg_placeholder(name.clone(), ty) {
                        self.errors.push(e.into());
                    }
                    self.tokens.push(Token::AppendToAttribute(Content::Placeholder(name)));
                }
            },
            CoreToken::Text(content) => match content {
                CoreContent::String(content) => self.tokens.push(Token::Text(Content::String(content))),
                CoreContent::Placeholder(name, ty) => {
                    if let Err(e) = self.reg_placeholder(name.clone(), ty) {
                        self.errors.push(e.into());
                    }
                    self.tokens.push(Token::Text(Content::Placeholder(name)));
                }
            },
            CoreToken::EndAttribute => self.tokens.push(Token::EndAttribute),
            CoreToken::Scope(scope) => {
                if let Err(e) = self.reg_scope_vars(&scope) {
                    self.errors.push(e.into());
                }
                self.tokens.push(Token::Scope(scope));
            },
            CoreToken::End => {
                if let Err(e) = self.end_scope() {
                    self.errors.push(e.into());
                }
                self.tokens.push(Token::End);
            },
            CoreToken::TypeHint(path, ty) => if let Err(e) = self.reg_placeholder(path, ty) {
                self.errors.push(e.into());
            },
        }
    }

    fn fragments(&self) -> &FragmentStore {
        &self.fragments
    }
}

enum ExtensibleMap<'a, K: 'a, V: 'a> {
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

impl<'a, K: Hash + Eq + Borrow<str> + From<&'static str>> FragmentStore for ExtensibleMap<'a, K, Box<Fragment>> {
    fn get(&self, ident: &str) -> Option<&Fragment> {
        self.get(ident).map(AsRef::as_ref)
    }

    fn insert(&mut self, fragment: Box<Fragment>) {
        self.insert(fragment.identifier().into(), fragment);
    }
}

fn to_camel_case(s: &str) -> String {
    s.split(|c: char| !c.is_alphanumeric()).flat_map(|word| word.chars().enumerate().map(|(i, c)|
        if i == 0 {
            c.to_uppercase().collect::<String>()
        } else {
            c.to_lowercase().collect()
        }
    )).collect::<Vec<_>>().concat()
}
