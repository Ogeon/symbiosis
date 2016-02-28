use std::io::{self, Write};
use std::collections::HashMap;
use std::default::Default;
use std::borrow::ToOwned;
use std::fmt;
use std::fmt::Write as FmtWrite;

use StrTendril;

use codegen::{Codegen, Writer, Line, Logic, Token, Scope, Content, Path, Structs, Type, Name};

#[derive(Debug)]
pub enum Error {
    NotSupported(&'static str),
    UnknownType(String),
    UndefinedPlaceholder(String),
    CannotBeText(String),
    CannotBeAttribute(String),
    UnexpectedType(String, Type),
    TooFewEnd,
    TooManyEnd,

    Io(io::Error)
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Error::NotSupported(feature) => write!(f, "{} is not supported by JavaScript templates", feature),
            &Error::UnknownType(ref placeholder) => write!(f, "the type of '{}' was not fully inferred", placeholder),
            &Error::UndefinedPlaceholder(ref name) => write!(f, "the placeholder '{}' was not defined", name),
            &Error::CannotBeText(ref name) => write!(f, "the placeholder '{}' cannot be rendered as text", name),
            &Error::CannotBeAttribute(ref name) => write!(f, "the placeholder '{}' cannot be rendered in an attribute", name),
            &Error::UnexpectedType(ref name, ref ty) => write!(f, "expected the placeholder '{}' to be of type '{}'", name, ty),
            &Error::TooFewEnd => write!(f, "unbalanced scopes: too few {{{{ end }}}}"),
            &Error::TooManyEnd => write!(f, "unbalanced scopes: too many {{{{ end }}}}"),

            &Error::Io(ref e) => e.fmt(f),
            //&Error::Fmt(ref e) => e.fmt(f)
        }
    }
}

impl From<io::Error> for Error {
    fn from(error: io::Error) -> Error {
        Error::Io(error)
    }
}

///Represents the state of a JavaScript variable.
pub enum VarState {
    ///Not predefined. Will be prefixed with `var`.
    ///Example: `var a = ...; use(a);`.
    New,
    ///Expected to be initialized somewhere else.
    ///Example: `use(a);`.
    Inited,
    ///Previously uninitialized, but there is no need for `var`.
    ///Example: `a = ...; use(a);`.
    Uninited
}

///Code generator for JavaScript templates.
pub struct JavaScript<'a> {
    ///The state of the template or namespace variable. Defaults to `New`.
    pub variable_state: VarState,

    ///The name of the namespace in which the template should be created, if
    ///any. Defaults to none.
    pub namespace: Option<&'a str>
}

impl<'a> JavaScript<'a> {
    fn eval_logic<W: Write>(&self, w: &mut Line<W>, first: bool, cond: &Logic, params: &HashMap<Name, Type>, structs: Structs, scopes: &[Option<ForEachScope<'a>>]) -> Result<(), Error> {
        match cond {
            &Logic::And(ref conds) => {
                if !first {
                    try_w!(w, "(");
                }
                for (i, cond) in conds.iter().enumerate() {
                    if i > 0 {
                        try_w!(w, " && ");
                    }
                    try!(self.eval_logic(w, false, cond, params, structs, scopes));
                }
                if !first {
                    try_w!(w, ")");
                }
            },
            &Logic::Or(ref conds) => {
                if !first {
                    try_w!(w, "(");
                }
                for (i, cond) in conds.iter().enumerate() {
                    if i > 0 {
                        try_w!(w, " || ");
                    }
                    try!(self.eval_logic(w, false, cond, params, structs, scopes));
                }
                if !first {
                    try_w!(w, ")");
                }
            },
            &Logic::Not(ref cond) => {
                try_w!(w, "!(");
                try!(self.eval_logic(w, false, cond, params, structs, scopes));
                try_w!(w, ")");
            },
            &Logic::Value(ref val) => {
                if let Some((path, _)) = find_param(val, params, structs, scopes) {
                    try_w!(w, "{}", path);
                } else {
                    return Err(Error::UndefinedPlaceholder(val.to_string()));
                }
            }
        }

        Ok(())
    }
}

impl<'a> Default for JavaScript<'a> {
    fn default() -> JavaScript<'a> {
        JavaScript {
            variable_state: VarState::New,
            namespace: None
        }
    }
}

impl<'a> Codegen for JavaScript<'a> {
    type Error = Error;

    fn init_writer<'w, W: Write>(&self, w: &'w mut W) -> Writer<'w, W> {
        Writer::new(w, "    ")
    }

    fn build_structs<W: Write>(&self, _w: &mut Writer<W>, _structs: Structs) -> Result<(), Self::Error> {
        Ok(())
    }

    fn build_template<W: Write>(&self, w: &mut Writer<W>, name: &str, params: &HashMap<Name, Type>, structs: Structs, tokens: &[Token]) -> Result<(), Error> {
        if let Some(namespace) = self.namespace {
            try_w!(w, "{}.{} = function() {{", namespace, name);
        } else {
            match self.variable_state {
                VarState::New => try_w!(w, "var {} = function() {{", name),
                VarState::Inited | VarState::Uninited => try_w!(w, "{} = function() {{", name),
            }
        }

        {
            let mut block = w.block();
            for (parameter, ty) in params {
                match ty {
                    &Type::Content(false) => try_w!(block, "this.{} = \"\";", parameter),
                    &Type::Content(true) => try_w!(block, "this.{} = null;", parameter),
                    &Type::Bool => try_w!(block, "this.{} = false;", parameter),
                    &Type::Collection(_, false) => try_w!(block, "this.{} = [];", parameter),
                    &Type::Collection(_, true) => try_w!(block, "this.{} = null;", parameter),
                    &Type::Struct(_, false) => try_w!(block, "this.{} = {{}};", parameter),
                    &Type::Struct(_, true) => try_w!(block, "this.{} = null;", parameter),
                }
            }
        }

        try_w!(w, "}};");


        if let Some(namespace) = self.namespace {
            try_w!(w, "{}.{}.prototype.render_to = function(root) {{", namespace, name);
        } else {
            try_w!(w, "{}.prototype.render_to = function(root) {{", name);
        }

        let mut tree_stack = vec![TokenTree {
            inherit_text: false,
            tokens: vec![]
        }];

        for token in tokens {
            match token {
                &Token::SetDoctype(_) => return Err(Error::NotSupported("doctype declarations")),
                &Token::Comment(_) => {},
                t @ &Token::BeginTag(_) | t @ &Token::EndTag(_) | t @ &Token::CloseTag(_) => {
                    if let Some(ref mut tree) = tree_stack.last_mut() {
                        tree.inherit_text = false;
                        tree.tokens.push(JsToken::Statement((t, false)));
                    }
                },
                t @ &Token::BeginAttribute(_, _) |
                t @ &Token::AppendToAttribute(_) |
                t @ &Token::EndAttribute |
                t @ &Token::Text(_) => {
                    if let Some(ref mut tree) = tree_stack.last_mut() {
                        tree.tokens.push(JsToken::Statement((t, false)));
                    }
                },
                t @ &Token::Scope(Scope::If(_)) => {
                    if let Some(ref mut tree) = tree_stack.last_mut() {
                        tree.tokens.push(JsToken::Statement((t, false)));
                    }
                    tree_stack.push(TokenTree {
                        inherit_text: true,
                        tokens: vec![]
                    });
                },
                t @ &Token::Scope(Scope::ForEach(_, _, _)) => {
                    if let Some(ref mut tree) = tree_stack.last_mut() {
                        tree.tokens.push(JsToken::Statement((t, false)));
                    }
                    tree_stack.push(TokenTree {
                        inherit_text: false,
                        tokens: vec![]
                    });
                },
                t @ &Token::End => {
                    if let Some(tree) = tree_stack.pop() {
                        if let Some(ref mut parent) = tree_stack.last_mut() {
                            if !tree.inherit_text {
                                if let Some(&mut JsToken::Statement((_, ref mut force))) = parent.tokens.last_mut() {
                                    *force = true;
                                }
                            }
                            parent.tokens.push(JsToken::Scope(tree));
                            parent.tokens.push(JsToken::Statement((t, false)));
                        }
                    }
                },
            }
        }

        if let Some(tree) = tree_stack.pop() {
            let mut func = w.block();

            if tree_stack.len() > 0 {
                return Err(Error::TooFewEnd);
            }

            let mut tags = vec!["root".to_owned()];
            let mut text: Vec<Option<(String, TextState)>> = vec![None];
            let mut attribute_var = ("".into(), String::new());
            let mut var_counter = 0u32;
            let mut scopes = vec![];

            try!(tree.traverse(&mut |token, inherit_text, force_append| {
                if force_append {
                    if let (Some(&mut Some((ref text_var, ref mut state))), Some(tag)) = (text.last_mut(), tags.last()) {
                        try!(append_text(&mut func, text_var, state, tag));
                    }
                }

                match token {
                    &Token::SetDoctype(_) => return Err(Error::NotSupported("doctype declarations")),
                    &Token::Comment(_) => {},
                    &Token::BeginTag(ref name) => {
                        if let (Some(&mut Some((ref text_var, ref mut state))), Some(tag)) = (text.last_mut(), tags.last()) {
                            try!(append_text(&mut func, text_var, state, tag));
                        }

                        let var = format!("tag_{}_{}", to_valid_ident(name), var_counter);
                        var_counter += 1;
                        try_w!(func, "var {} = document.createElement(\"{}\");", var, name);
                        tags.push(var);
                    },
                    &Token::EndTag(self_closing) => if self_closing {
                        if let (Some(child), Some(parent)) = (tags.pop(), tags.last()) {
                            try_w!(func, "{}.appendChild({});", parent, child)
                        }
                    },
                    &Token::CloseTag(_) => {
                        if let (Some(&mut Some((ref text_var, ref mut state))), Some(tag)) = (text.last_mut(), tags.last()) {
                            try!(append_text(&mut func, text_var, state, tag));
                        }

                        if let (Some(child), Some(parent)) = (tags.pop(), tags.last()) {
                            try_w!(func, "{}.appendChild({});", parent, child)
                        }
                    },
                    &Token::BeginAttribute(ref name, ref content) => {
                        attribute_var = (name.clone(), format!("attr_{}_{}", to_valid_ident(name), var_counter));
                        var_counter += 1;
                        try!(write_attribute(&mut func, &attribute_var.1, content, true, params, structs, &scopes));
                    },
                    &Token::AppendToAttribute(ref content) => {
                        try!(write_attribute(&mut func, &attribute_var.1, content, false, params, structs, &scopes));
                    },
                    &Token::EndAttribute => {
                        if let Some(tag) = tags.last() {
                            try_w!(func, "{}.setAttribute(\"{}\", {});", tag, attribute_var.0, attribute_var.1);
                        }
                    },
                    &Token::Text(ref content) => {
                        if let Some(text) = text.last_mut() {
                            if text.is_none() && !inherit_text {
                                let text_var = format!("text_{}", var_counter);
                                var_counter += 1;
                                try_w!(func, "var {} = \"\";", text_var);
                                *text = Some((text_var, TextState::Cleared));
                            }
                        }

                        for text in text.iter_mut().rev() {
                            if let (&mut Some((ref text_var, ref mut state)), Some(tag)) = (text, tags.last()) {
                                if state.has_leftovers() {
                                    *state = TextState::HasContent;
                                    try_w!(func, "{} = \"\";", text_var);
                                }
                                try!(write_text(&mut func, text_var, state, content, tag, params, structs, &scopes));
                                break;
                            }
                        }
                    },
                    &Token::Scope(Scope::If(ref cond)) => {
                        if let Some(text) = text.last_mut() {
                            if text.is_none() && !inherit_text {
                                let text_var = format!("text_{}", var_counter);
                                var_counter += 1;
                                try_w!(func, "var {} = \"\";", text_var);
                                *text = Some((text_var, TextState::Cleared));
                            } else if let &mut Some((ref text_var, ref mut state)) = text {
                                if state.has_leftovers() {
                                    try_w!(func, "{} = \"\";", text_var);
                                    *state = TextState::Cleared;
                                }
                            }
                        }

                        text.push(None);

                        {
                            let mut line = func.begin_line();
                            try_w!(line, "if(");
                            try!(self.eval_logic(&mut line, true, &cond.flattened(), params, structs, &scopes));
                            try_w!(line, ") {{");
                        }
                        func.indent();
                        scopes.push(None);
                    },
                    &Token::Scope(Scope::ForEach(ref collection, ref element, ref opt_key)) => {
                        if let (Some(&mut Some((ref text_var, ref mut state))), Some(tag)) = (text.last_mut(), tags.last()) {
                            try!(append_text(&mut func, text_var, state, tag));
                            try_w!(func, "{} = \"\";", text_var);
                            *state = TextState::Cleared;
                        }

                        if let Some(text) = text.last_mut() {
                            if text.is_none() && !inherit_text {
                                let text_var = format!("text_{}", var_counter);
                                var_counter += 1;
                                try_w!(func, "var {} = \"\";", text_var);
                                *text = Some((text_var, TextState::Cleared));
                            }
                        }

                        text.push(None);

                        let key = match opt_key {
                            &Some(ref k) => format!("key_{}_{}", to_valid_ident(k), var_counter),
                            &None => format!("key_{}", var_counter)
                        };
                        let value = format!("elem_{}_{}", to_valid_ident(element), var_counter);

                        let (collection_name, ty) = match find_param(collection, params, structs, &scopes) {
                            Some((ref name, Some(&Type::Collection(ref ty, _)))) => (name.clone(), ty),
                            Some((_, _)) => return Err(Error::UnexpectedType(collection.to_string(), Type::Collection(None, false))),
                            None => return Err(Error::UndefinedPlaceholder(collection.to_string()))
                        };

                        var_counter += 1;

                        try_w!(func, "var {};", key);
                        try_w!(func, "for({} in {}) {{", key, collection_name);
                        func.indent();
                        try_w!(func, "if({}.hasOwnProperty({})) {{", collection_name, key);
                        func.indent();
                        try_w!(func, "var {} = {}[{}];", value, collection_name, key);
                        if opt_key.is_some() {
                            try_w!(func, "if(!isNaN({})) {{", key);
                            try_w!(func.indented_line(), "{}++;", key);
                            try_w!(func, "}}");
                        }

                        if let &Some(ref ty) = ty {
                            scopes.push(Some( ForEachScope {
                                variable: element,
                                alias: value.into(),
                                ty: ty,
                                key: opt_key.clone().map(|k| (k, key))
                            }));
                        } else {
                            return Err(Error::UnknownType(collection.to_string()));
                        }
                    },
                    &Token::End => {
                        if let (Some(Some((ref text_var, ref mut state))), Some(tag)) = (text.pop(), tags.last()) {
                            try!(append_text(&mut func, text_var, state, tag));
                        }
                        func.unindent();
                        try_w!(func, "}}");
                        if let Some(Some(_)) = scopes.pop() {
                            func.unindent();
                            try_w!(func, "}}");
                        }
                    },
                }

                Ok(())
            }));
        } else {
            return Err(Error::TooManyEnd);
        }

        try_w!(w, "}};");
        Ok(())
    }

    fn build_module<W, F>(&self, w: &mut Writer<W>, build_templates: F) -> Result<(), Error> where
        W: Write,
        F: FnOnce(&mut Writer<W>) -> Result<(), Error>
    {
        if let Some(ref namespace) = self.namespace {
            match self.variable_state {
                VarState::New => try_w!(w, "var {} = {{}};", namespace),
                VarState::Uninited => try_w!(w, "{} = {{}};", namespace),
                _ => {}
            }
        }

        build_templates(w)
    }
}

struct TokenTree<'a> {
    inherit_text: bool,
    tokens: Vec<JsToken<'a>>
}

impl<'a> TokenTree<'a> {
    fn traverse<F: FnMut(&'a Token, bool, bool) -> Result<(), Error>>(self, f: &mut F) -> Result<(), Error> {
        for token in self.tokens {
            match token {
                JsToken::Statement((token, force_append)) => try!(f(token, self.inherit_text, force_append)),
                JsToken::Scope(tree) => try!(tree.traverse(f))
            }
        }

        Ok(())
    }
}

enum JsToken<'a> {
    Statement((&'a Token, bool)),
    Scope(TokenTree<'a>)
}

enum TextState {
    Cleared,
    HasContent,
    Leftovers
}

impl TextState {
    fn has_content(&self) -> bool {
        match self {
            &TextState::HasContent => true,
            _ => false
        }
    }
    fn has_leftovers(&self) -> bool {
        match self {
            &TextState::Leftovers => true,
            _ => false
        }
    }
}

fn to_valid_ident(name: &str) -> String {
    name.chars().map(|c| if !c.is_alphanumeric() { '_' } else { c }).collect()
}

fn find_param<'a, 'b>(path: &Path, params: &'a HashMap<Name, Type>, structs: Structs<'a>, scopes: &'b [Option<ForEachScope<'a>>]) -> Option<(String, Option<&'a Type>)> {
    let param = path.first().expect("found empty path");

    println!("looking for '{}' in {} scopes", param, scopes.len());
    for scope in scopes.iter().rev() {
        if let &Some(ForEachScope { ref variable, ref alias, ref ty, ref key }) = scope {
            println!("name: '{}', param: '{}'", variable, param);
            if *variable == &**param {
                let access_path = vec![alias];
                return make_access_path(path, access_path, ty, structs);
            } else if let &Some((ref key, ref alias)) = key {
                if key == param {
                    return Some((alias.clone(), None))
                }
            }
        }
    }

    params.get(&**param).and_then(|t| {
        println!("global type of '{}' is {}", param, t);
        make_access_path(path, vec![&"this".into(), param], t, structs)
    })
}

fn make_access_path<'a>(path: &Path, base: Vec<&StrTendril>, base_type: &'a Type, structs: Structs<'a>) -> Option<(String, Option<&'a Type>)> {
    let mut access_path = base;
    let mut current_type = Some(base_type);

    if path.len() > 1 {
        let mut current_params = match *base_type {
            Type::Struct(ref name, _) => structs.get(name).map(|s| &s.fields),
            _ => return None,
        };
        let mut path = path[1..].iter();

        while let (Some(params), Some(part)) = (current_params.take(), path.next()) {
            let next_type = if let Some(ty) = params.get(&**part) {
                ty
            } else {
                return None;
            };

            access_path.push(part);
            current_params = match *next_type {
                Type::Struct(ref name, _) => structs.get(name).map(|s| &s.fields),
                _ => None,
            };
            current_type = Some(next_type);
        }
    }

    if let Some(ty) = current_type {
        let mut access_path = access_path.into_iter().rev();
        let mut path = access_path.next().map(|name| name.into()).unwrap_or(String::new());

        for name in access_path {
            path = format!("{}.{}", name, path);
        }

        Some((path, Some(ty)))
    } else {
        None
    }
}

fn append_text<W: Write>(w: &mut Writer<W>, var: &str, state: &mut TextState, tag: &str) -> Result<(), Error> {
    if state.has_content() {
        try_w!(w, "{}.appendChild(document.createTextNode({}));", tag, var);
        *state = TextState::Leftovers;
    }

    Ok(())
}

fn write_attribute<'a, W: Write>(
    w: &mut Writer<W>,
    var: &str,
    content: &Content,
    new: bool,
    params: &'a HashMap<Name, Type>,
    structs: Structs<'a>,
    scopes: &[Option<ForEachScope<'a>>]
) -> Result<(), Error> {
    match content {
        &Content::String(ref content) => {
            if new {
                try_w!(w, "var {} = \"{}\";", var, content);
            } else if !content.is_empty() {
                try_w!(w, "{} += \"{}\";", var, content);
            }
        },
        &Content::Placeholder(ref placeholder) => {
            match find_param(placeholder, params, structs, &scopes) {
                Some((path, Some(&Type::Content(_)))) | Some((path, Some(&Type::Bool))) | Some((path, None)) => {
                    if new {
                        try_w!(w, "var {} = \"\";", var);
                    }
                    try_w!(w, "if({} !== null) {{", path);
                    try_w!(w.indented_line(), "{} += {};", var, path);
                    try_w!(w, "}}");
                },
                Some((_, Some(&Type::Collection(_, _)))) | Some((_, Some(&Type::Struct(_, _)))) => {
                    return Err(Error::CannotBeAttribute(placeholder.to_string()));
                },
                None => return Err(Error::UndefinedPlaceholder(placeholder.to_string()))
            }
        }
    }

    Ok(())
}

fn write_text<'a, W: Write>(
    w: &mut Writer<W>,
    var: &str,
    state: &mut TextState,
    content: &Content,
    _parent: &str,
    params: &'a HashMap<Name, Type>,
    structs: Structs<'a>,
    scopes: &[Option<ForEachScope<'a>>]
) -> Result<(), Error> {
    match content {
        &Content::String(ref content) => if !content.is_empty() {
            try_w!(w, "{} += \"{}\";", var, Sanitized(content.to_string()));
            *state = TextState::HasContent;
        },
        &Content::Placeholder(ref placeholder) => {
            match find_param(placeholder, params, structs, scopes) {
                Some((path, Some(&Type::Content(_)))) | Some((path, Some(&Type::Bool))) | Some((path, None)) => {
                    try_w!(w, "if({} !== null) {{", path);
                    try_w!(w.indented_line(), "{} += {};", var, path);
                    try_w!(w, "}}");
                    *state = TextState::HasContent;
                },
                /*Some((alias, Some(&ContentType::Template(_)))) => {
                    try!(append_text(w, var, state, parent));
                    match alias {
                        Some(alias) => {
                            try_w!(w, "if({} !== null) {{", alias);
                        },
                        None => {
                            try_w!(w, "if(this.{} !== null) {{", placeholder);
                        }
                    }
                    {
                        let mut line = match alias {
                            Some(alias) => {
                                try_w!(w, "if({} !== null) {{", alias);
                                let mut line = w.begin_line();
                                try_w!(line, "{}", alias);
                                line
                            },
                            None => {
                                try_w!(w, "if(this.{} !== null) {{", placeholder);
                                let mut line = w.begin_line();
                                try_w!(line, "this.{}", placeholder);
                                line
                            }
                        };
                        try_w!(line, ".render_to({});", parent);
                    }
                    try_w!(w, "}}");
                },*/
                Some((_, Some(&Type::Collection(_, _)))) | Some((_, Some(&Type::Struct(_, _)))) => {
                    return Err(Error::CannotBeText(placeholder.to_string()));
                },
                None => return Err(Error::UndefinedPlaceholder(placeholder.to_string()))
            }
        }
    }

    Ok(())
}

struct ForEachScope<'a> {
    variable: &'a str,
    alias: StrTendril,
    ty: &'a Type,
    key: Option<(StrTendril, String)>
}

struct Sanitized<S>(S);

impl<S> fmt::Display for Sanitized<S> where S: AsRef<str> {
    fn fmt(&self, f: &mut fmt::Formatter) ->  fmt::Result {
        for c in self.0.as_ref().chars() {
            match c {
                '&' => try!(f.write_str("&amp;")),
                '<' => try!(f.write_str("&lt;")),
                '>' => try!(f.write_str("&gt;")),
                '"' => try!(f.write_str("&quot;")),
                '\'' => try!(f.write_str("&#39;")),
                '\n' => try!(f.write_str("\\n")),
                '\t' => try!(f.write_str("\\t")),
                c => try!(f.write_char(c))
            }
        }

        Ok(())
    }
}
