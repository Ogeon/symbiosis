extern crate symbiosis_tokenizer;
extern crate serde_json;

use std::collections::{HashMap, BTreeMap};
use std::fmt::{self, Write, Display};
use std::mem::replace;

use serde_json::Value;

pub use symbiosis_tokenizer::StrTendril;
use symbiosis_tokenizer::{Tokenizer, TokenSink, Error};
use symbiosis_tokenizer::codegen::Token as CoreToken;
use symbiosis_tokenizer::codegen::Scope as CoreScope;
use symbiosis_tokenizer::codegen::Logic as CoreLogic;
use symbiosis_tokenizer::codegen::{Path, Content};
use symbiosis_tokenizer::fragment::{Fragment, FragmentStore};
use symbiosis_tokenizer::fragment::pattern::Argument;

pub use symbiosis_tokenizer::fragment;

pub struct Template {
    pub content: BTreeMap<String, Value>,
    tokens: Vec<Token>,
}

impl Template {
    fn find<'a>(&'a self, path: &AccessPath, vars: Option<&'a ScopeVars>) -> Option<&Value> {
        match path.kind {
            PathKind::Global => {
                let mut current_params = Some(&self.content);
                let mut current_value = None;

                for part in &path.path {
                    if let Some(params) = current_params.take() {
                        current_value = params.get(&**part);
                        current_params = current_value.and_then(Value::as_object);
                    } else {
                        return None;
                    }
                }

                current_value
            },
            PathKind::Local => {
                let first = path.path.first().expect("found an empty path");
                let mut current_scope = vars;

                while let Some(scope) = current_scope.take() {
                    let value = if &scope.value.0 == first {
                        Some(scope.value.1)
                    } else if let Some((ref name, ref value)) = scope.key {
                        if name == first {
                            Some(value)
                        } else {
                            None
                        }
                    } else {
                        None
                    };

                    if let Some(value) = value {
                        let mut current_params = value.as_object();
                        let mut current_value = None;

                        for part in &path.path[1..] {
                            if let Some(params) = current_params.take() {
                                current_value = params.get(&**part);
                                current_params = current_value.and_then(Value::as_object);
                            } else {
                                return None;
                            }
                        }

                        return current_value;
                    }

                    current_scope = scope.previous;
                }

                None
            }
        }
    }
}

impl fmt::Display for Template {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for token in &self.tokens {
            match *token {
                Token::Raw(ref s) => try!(s.fmt(f)),
                Token::Content(ref path) => match self.find(&path, None) {
                    Some(&Value::String(ref x)) => try!(x.fmt(f)),
                    Some(&Value::I64(x)) => try!(x.fmt(f)),
                    Some(&Value::U64(x)) => try!(x.fmt(f)),
                    Some(&Value::F64(x)) => try!(x.fmt(f)),
                    Some(&Value::Bool(x)) => try!(x.fmt(f)),
                    Some(&Value::Array(_)) |
                    Some(&Value::Object(_)) |
                    Some(&Value::Null)|
                    None => {},
                },
                Token::Scope(ref scope) => try!(scope.display(self, None, f)),
            }
        }

        Ok(())
    }
}

pub struct Parser {
    fragments: HashMap<&'static str, Box<Fragment>>,
}

impl Parser {
    pub fn new() -> Parser {
        Parser {
            fragments: HashMap::new(),
        }
    }

    pub fn parse_string<S: Into<StrTendril>>(&self, source: S) -> Result<Template, Error> {
        let mut tokens = TokenBuilder::new(&self.fragments);
        {
            let mut tokenizer = Tokenizer::new(&mut tokens);
            try!(tokenizer.parse_string(source.into()));
        }

        Ok(Template {
            content: BTreeMap::new(),
            tokens: tokens.into(),
        })
    }

    pub fn register_fragment<F: Fragment + 'static>(&mut self, fragment: F) {
        self.fragments.insert(fragment.identifier(), Box::new(fragment));
    }

    pub fn register_fragment_fn(&mut self, name: &'static str, function: fn(Vec<Argument>) -> Result<fragment::ReturnType, fragment::Error>) {
        self.register_fragment((name, function));
    }
}

struct TokenBuilder<'a> {
    string_buf: String,
    tokens: Vec<Token>,
    scopes: Vec<Scope>,
    fragments: &'a FragmentStore,
}

impl<'a> TokenBuilder<'a> {
    fn new(fragments: &'a FragmentStore) -> TokenBuilder<'a> {
        TokenBuilder {
            string_buf: String::new(),
            tokens: vec![],
            scopes: vec![],
            fragments: fragments,
        }
    }

    fn find(&self, path: Path) -> AccessPath {
        {
            let first = path.first().expect("found an empty path");

            for &Scope { ref kind, .. } in self.scopes.iter().rev() {
                if let ScopeKind::ForEach { ref key, ref alias, .. } = *kind {
                    if alias == first {
                        return AccessPath {
                            path: path[1..].to_owned().into(),
                            kind: PathKind::Local,
                        };
                    } else if let Some(ref key) = *key {
                        if key == first {
                            return AccessPath {
                                path: path[1..].to_owned().into(),
                                kind: PathKind::Local,
                            };
                        }
                    }
                }
            }
        }

        AccessPath {
            path: path,
            kind: PathKind::Global,
        }
    }

    fn push_token(&mut self, token: Token) {
        if !self.string_buf.is_empty() {
            let s = replace(&mut self.string_buf, String::new());
            self.push_token(Token::Raw(s));
        }

        if let Some(&mut Scope {ref mut tokens, .. }) = self.scopes.last_mut() {
            tokens.push(token);
        } else {
            self.tokens.push(token);
        }
    }

    fn open_scope(&mut self, kind: ScopeKind) {
        if !self.string_buf.is_empty() {
            let s = replace(&mut self.string_buf, String::new());
            self.push_token(Token::Raw(s));
        }

        self.scopes.push(Scope { kind: kind, tokens: vec![] });
    }

    fn close_scope(&mut self) {
        if !self.string_buf.is_empty() {
            let s = replace(&mut self.string_buf, String::new());
            self.push_token(Token::Raw(s));
        }

        if let Some(scope) = self.scopes.pop() {
            self.push_token(Token::Scope(scope));
        }
    }
}

impl<'a, 'b> TokenSink for &'b mut TokenBuilder<'a> {
    fn process_token(&mut self, token: CoreToken) {
        match token {
            CoreToken::SetDoctype(doctype) => {
                self.string_buf.push_str("<!DOCTYPE");
                if let Some(ref name) = doctype.name {
                    self.string_buf.push_str(name);
                }

                if let Some(ref public_id) = doctype.public_id {
                    write!(&mut self.string_buf, " PUBLIC \"{}\"", public_id).unwrap();
                } else if doctype.system_id.is_some() {
                    self.string_buf.push_str(" SYSTEM");
                }

                if let Some(ref system_id) = doctype.system_id {
                    write!(&mut self.string_buf, " \"{}\"", system_id).unwrap();
                }
                self.string_buf.push_str(">");
            },
            CoreToken::Comment(comment) => write!(&mut self.string_buf, "<!--{}-->", comment).unwrap(),
            CoreToken::BeginTag(name) => write!(&mut self.string_buf, "<{}", name).unwrap(),
            CoreToken::EndTag(_self_close) => self.string_buf.push_str(">"),
            CoreToken::CloseTag(name) => write!(&mut self.string_buf, "</{}>", name).unwrap(),
            CoreToken::BeginAttribute(name, content) => match content {
                Content::String(content) => write!(&mut self.string_buf, " {}=\"{}", name, content).unwrap(),
                Content::Placeholder(placeholder, _) => {
                    write!(&mut self.string_buf, " {}=\"", name).unwrap();
                    let path = self.find(placeholder);
                    self.push_token(Token::Content(path));
                }
            },
            CoreToken::AppendToAttribute(content) | CoreToken::Text(content) => match content {
                Content::String(content) => self.string_buf.push_str(&content.to_string()),
                Content::Placeholder(placeholder, _) => {
                    let path = self.find(placeholder);
                    self.push_token(Token::Content(path));
                }
            },
            CoreToken::EndAttribute => self.string_buf.push_str("\""),
            CoreToken::Scope(CoreScope::If(logic)) => {
                let logic = Logic::from_core(self, logic);
                self.open_scope(ScopeKind::If(logic));
            },
            CoreToken::Scope(CoreScope::ForEach(collection, alias, key)) => {
                let path = self.find(collection);
                self.open_scope(ScopeKind::ForEach {
                    key: key,
                    alias: alias,
                    collection: path,
                });
            },
            CoreToken::End => self.close_scope(),
        }
    }

    fn fragments(&self) -> &FragmentStore {
        self.fragments
    }
}

impl<'a> Into<Vec<Token>> for TokenBuilder<'a> {
    fn into(mut self) -> Vec<Token> {
        while !self.scopes.is_empty() {
            self.close_scope();
        }

        if !self.string_buf.is_empty() {
            let s = replace(&mut self.string_buf, String::new());
            self.push_token(Token::Raw(s));
        }

        self.tokens
    }
}

enum Token {
    Raw(String),
    Content(AccessPath),
    Scope(Scope),
}

struct Scope {
    kind: ScopeKind,
    tokens: Vec<Token>,
}

impl Scope {
    fn display(&self, template: &Template, vars: Option<&ScopeVars>, f: &mut fmt::Formatter) -> fmt::Result {
        match self.kind {
            ScopeKind::If(ref logic) => if logic.evaluate(template, vars) {
                return self.display_tokens(template, vars, f)
            },
            ScopeKind::ForEach { ref key, ref alias, ref collection } => {
                let c = template.find(collection, vars);
                match (c, key) {
                    (Some(&Value::Object(ref o)), &None) =>  for value in o.values() {
                        try!(self.display_tokens(&*template, Some(&ScopeVars {
                            key: None,
                            value: (alias.clone(), value),
                            previous: vars,
                        }), f));
                    },
                    (Some(&Value::Object(ref o)), &Some(ref key)) =>  for (k, value) in o {
                        try!(self.display_tokens(&*template, Some(&ScopeVars {
                            key: Some((key.clone(), Value::String(k.clone()))),
                            value: (alias.clone(), value),
                            previous: vars,
                        }), f));
                    },
                    (Some(&Value::Array(ref a)), &None) =>  for value in a {
                        try!(self.display_tokens(&*template, Some(&ScopeVars {
                            key: None,
                            value: (alias.clone(), value),
                            previous: vars,
                        }), f));
                    },
                    (Some(&Value::Array(ref a)), &Some(ref key)) =>  for (k, value) in a.iter().enumerate() {
                        try!(self.display_tokens(&*template, Some(&ScopeVars {
                            key: Some((key.clone(), Value::U64(k as u64 + 1))),
                            value: (alias.clone(), value),
                            previous: vars,
                        }), f));
                    },
                    _ => {}
                }
            },
        }

        Ok(())
    }

    fn display_tokens(&self, template: &Template, vars: Option<&ScopeVars>, f: &mut fmt::Formatter) -> fmt::Result {
        for token in &self.tokens {
            match *token {
                Token::Raw(ref s) => try!(s.fmt(f)),
                Token::Content(ref path) => match template.find(&path, vars) {
                    Some(&Value::String(ref x)) => try!(x.fmt(f)),
                    Some(&Value::I64(x)) => try!(x.fmt(f)),
                    Some(&Value::U64(x)) => try!(x.fmt(f)),
                    Some(&Value::F64(x)) => try!(x.fmt(f)),
                    Some(&Value::Bool(x)) => try!(x.fmt(f)),
                    Some(&Value::Array(_)) |
                    Some(&Value::Object(_)) |
                    Some(&Value::Null)|
                    None => {},
                },
                Token::Scope(ref scope) => try!(scope.display(template, vars, f)),
            }
        }

        Ok(())
    }
}

enum ScopeKind {
    If(Logic),
    ForEach {
        key: Option<StrTendril>,
        alias: StrTendril,
        collection: AccessPath
    }
}

struct AccessPath {
    path: Path,
    kind: PathKind,
}

#[derive(Clone, Copy)]
enum PathKind {
    Local,
    Global,
}

struct ScopeVars<'a> {
    key: Option<(StrTendril, Value)>,
    value: (StrTendril, &'a Value),
    previous: Option<&'a ScopeVars<'a>>
}

enum Logic {
    And(Vec<Logic>),
    Or(Vec<Logic>),
    Not(Box<Logic>),
    Value(AccessPath),
}

impl Logic {
    fn from_core(builder: &TokenBuilder, logic: CoreLogic) -> Logic {
        match logic {
            CoreLogic::And(v) => Logic::And(v.into_iter().map(|x| Logic::from_core(builder, x)).collect()),
            CoreLogic::Or(v) => Logic::Or(v.into_iter().map(|x| Logic::from_core(builder, x)).collect()),
            CoreLogic::Not(x) => Logic::Not(Box::new(Logic::from_core(builder, *x))),
            CoreLogic::Value(x) => Logic::Value(builder.find(x)),
        }
    }

    fn evaluate(&self, template: &Template, vars: Option<&ScopeVars>) -> bool {
        match *self {
            Logic::And(ref v) => v.iter().map(|x| x.evaluate(template, vars)).all(|x| x),
            Logic::Or(ref v) => v.iter().map(|x| x.evaluate(template, vars)).any(|x| x),
            Logic::Not(ref x) => !x.evaluate(template, vars),
            Logic::Value(ref x) => match template.find(x, vars) {
                Some(&Value::String(_)) | Some(&Value::I64(_)) | Some(&Value::U64(_)) | Some(&Value::F64(_)) |
                Some(&Value::Object(_)) | Some(&Value::Bool(true)) | Some(&Value::Array(_))  => true,
                None | Some(&Value::Bool(false)) | Some(&Value::Null) => false,
            },
        }
    }
}
