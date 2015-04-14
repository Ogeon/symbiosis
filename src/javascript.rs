use std::io::Write;
use std::collections::HashMap;
use std::default::Default;
use std::borrow::{Cow, ToOwned};

use string_cache::atom::Atom;

use codegen::{Codegen, Logic, Token, Scope, ContentType, Content, write_indent};

use Error;

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
    fn eval_logic<W: Write>(&self, w: &mut W, first: bool, cond: &Logic, params: &HashMap<String, ContentType>) -> Result<(), Error> {
        match cond {
            &Logic::And(ref conds) => {
                if !first {
                    try!(write!(w, "("));
                }
                for (i, cond) in conds.iter().enumerate() {
                    if i > 0 {
                        try!(write!(w, " && "));
                    }
                    try!(self.eval_logic(w, false, cond, params));
                }
                if !first {
                    try!(write!(w, ")"));
                }
            },
            &Logic::Or(ref conds) => {
                if !first {
                    try!(write!(w, "("));
                }
                for (i, cond) in conds.iter().enumerate() {
                    if i > 0 {
                        try!(write!(w, " || "));
                    }
                    try!(self.eval_logic(w, false, cond, params));
                }
                if !first {
                    try!(write!(w, ")"));
                }
            },
            &Logic::Not(ref cond) => {
                try!(write!(w, "!("));
                try!(self.eval_logic(w, false, cond, params));
                try!(write!(w, ")"));
            },
            &Logic::Value(ref val) => {
                try!(write!(w, "this.{}", val));
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
    fn build_template<W: Write>(&self, w: &mut W, name: &str, mut indent: u8, params: &HashMap<String, ContentType>, tokens: &[Token]) -> Result<(), Error> {
        if let Some(namespace) = self.namespace {
            line!(w, indent, "{}.{} = function() {{", namespace, name);
        } else {
            match self.variable_state {
                VarState::New => line!(w, indent, "var {} = function() {{", name),
                VarState::Inited | VarState::Uninited => line!(w, indent, "{} = function() {{", name),
            }
        }

        for (parameter, ty) in params {
            match ty {
                &ContentType::String(false) => line!(w, indent + 1, "this.{} = \"\";", parameter),
                &ContentType::String(true) => line!(w, indent + 1, "this.{} = null;", parameter),
                &ContentType::Bool => line!(w, indent + 1, "this.{} = false;", parameter),
                &ContentType::Template(_) => line!(w, indent + 1, "this.{} = null;", parameter),
                &ContentType::Collection(_, false) => line!(w, indent + 1, "this.{} = [];", parameter),
                &ContentType::Collection(_, true) => line!(w, indent + 1, "this.{} = null;", parameter)
            }
        }

        line!(w, indent, "}};");


        if let Some(namespace) = self.namespace {
            line!(w, indent, "{}.{}.prototype.render_to = function(root) {{", namespace, name);
        } else {
            line!(w, indent, "{}.prototype.render_to = function(root) {{", name);
        }

        indent += 1;

        let mut tree_stack = vec![TokenTree {
            inherit_text: false,
            tokens: vec![]
        }];

        for token in tokens {
            match token {
                &Token::SetDoctype(_) => return Err(Error::Parse(vec![Cow::Borrowed("doctype declarations are currently not supported")])),
                t @ &Token::BeginTag(_) | t @ &Token::EndTag(_) | t @ &Token::CloseTag(_) => {
                    if let Some(ref mut tree) = tree_stack.last_mut() {
                        tree.inherit_text = false;
                        tree.tokens.push(JsToken::Statement((t, false)));
                    }
                },
                t @ &Token::BeginAttribute(_, _) |
                t @ &Token::AppendToAttribute(_) |
                t @ &Token::EndAttribute |
                t @ &Token::BeginText(_) |
                t @ &Token::AppendToText(_) |
                t @ &Token::EndText => {
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
                }
            }
        }

        if let Some(tree) = tree_stack.pop() {
            if tree_stack.len() > 0 {
                return Err(Error::Parse(vec![Cow::Borrowed("too few {{ end }}")]));
            }

            let mut tags = vec!["root".to_owned()];
            let mut text: Vec<Option<(String, TextState)>> = vec![None];
            let mut attribute_var = (Atom::from_slice(""), String::new());
            let mut var_counter = 0u32;
            let mut scopes = vec![];

            try!(tree.traverse(&mut |token, inherit_text, force_append| {
                if force_append {
                    if let (Some(&mut Some((ref text_var, ref mut state))), Some(tag)) = (text.last_mut(), tags.last()) {
                        try!(append_text(w, indent, text_var, state, tag));
                    }
                }

                match token {
                    &Token::SetDoctype(_) => return Err(Error::Parse(vec![Cow::Borrowed("doctype declarations are currently not supported")])),
                    &Token::BeginTag(ref name) => {
                        if let (Some(&mut Some((ref text_var, ref mut state))), Some(tag)) = (text.last_mut(), tags.last()) {
                            try!(append_text(w, indent, text_var, state, tag));
                        }

                        let var = format!("tag_{}_{}", to_valid_ident(name.as_slice()), var_counter);
                        var_counter += 1;
                        line!(w, indent, "var {} = document.createElement(\"{}\");", var, name.as_slice());
                        tags.push(var);
                    },
                    &Token::EndTag(self_closing) => if self_closing {
                        if let (Some(child), Some(parent)) = (tags.pop(), tags.last()) {
                            line!(w, indent, "{}.appendChild({});", parent, child)
                        }
                    },
                    &Token::CloseTag(_) => {
                        if let (Some(&mut Some((ref text_var, ref mut state))), Some(tag)) = (text.last_mut(), tags.last()) {
                            try!(append_text(w, indent, text_var, state, tag));
                        }

                        if let (Some(child), Some(parent)) = (tags.pop(), tags.last()) {
                            line!(w, indent, "{}.appendChild({});", parent, child)
                        }
                    },
                    &Token::BeginAttribute(ref name, ref content) => {
                        attribute_var = (name.clone(), format!("attr_{}_{}", to_valid_ident(name.as_slice()), var_counter));
                        var_counter += 1;
                        try!(write_attribute(w, indent, &attribute_var.1, content, true, params, &scopes));
                    },
                    &Token::AppendToAttribute(ref content) => {
                        try!(write_attribute(w, indent, &attribute_var.1, content, false, params, &scopes));
                    },
                    &Token::EndAttribute => {
                        if let Some(tag) = tags.last() {
                            line!(w, indent, "{}.setAttribute(\"{}\", {});", tag, attribute_var.0.as_slice(), attribute_var.1);
                        }
                    },
                    &Token::BeginText(ref content) | &Token::AppendToText(ref content) => {
                        if let Some(text) = text.last_mut() {
                            if text.is_none() && !inherit_text {
                                let text_var = format!("text_{}", var_counter);
                                var_counter += 1;
                                line!(w, indent, "var {} = \"\";", text_var);
                                *text = Some((text_var, TextState::Cleared));
                            }
                        }

                        for text in text.iter_mut().rev() {
                            if let (&mut Some((ref text_var, ref mut state)), Some(tag)) = (text, tags.last()) {
                                if state.has_leftovers() {
                                    *state = TextState::HasContent;
                                    line!(w, indent, "{} = \"\";", text_var);
                                }
                                try!(write_text(w, indent, text_var, state, content, tag, params, &scopes));
                                break;
                            }
                        }
                    },
                    &Token::EndText => {},
                    &Token::Scope(Scope::If(ref cond)) => {
                        if let Some(text) = text.last_mut() {
                            if text.is_none() && !inherit_text {
                                let text_var = format!("text_{}", var_counter);
                                var_counter += 1;
                                line!(w, indent, "var {} = \"\";", text_var);
                                *text = Some((text_var, TextState::Cleared));
                            }
                        }

                        text.push(None);

                        try!(write_indent(w, indent));
                        try!(write!(w, "if("));
                        try!(self.eval_logic(w, true, &cond.flattened(), params));
                        try!(write!(w, ") {{\n"));
                        indent += 1;
                        scopes.push(None);
                    },
                    &Token::Scope(Scope::ForEach(ref collection, ref element, ref opt_key)) => {
                        if let Some(text) = text.last_mut() {
                            if text.is_none() && !inherit_text {
                                let text_var = format!("text_{}", var_counter);
                                var_counter += 1;
                                line!(w, indent, "var {} = \"\";", text_var);
                                *text = Some((text_var, TextState::Cleared));
                            }
                        }

                        if let (Some(&mut Some((ref text_var, ref mut state))), Some(tag)) = (text.last_mut(), tags.last()) {
                            try!(append_text(w, indent, text_var, state, tag));
                        }

                        text.push(None);

                        let key = match opt_key {
                            &Some(ref k) => format!("key_{}_{}", to_valid_ident(k), var_counter),
                            &None => format!("key_{}", var_counter)
                        };
                        let value = format!("elem_{}_{}", to_valid_ident(element), var_counter);

                        let (collection_name, ty) = match find_param(collection, params, &scopes) {
                            Some((Some(ref name), Some(&ContentType::Collection(ref ty, _)))) => ((*name).to_owned(), ty),
                            Some((None, Some(&ContentType::Collection(ref ty, _)))) => (format!("this.{}", collection), ty),
                            Some((_, Some(ty))) => return Err(Error::Parse(vec![Cow::Owned(format!("expected {} to be a collection, but found {}", collection, ty))])),
                            Some((_, None)) => return Err(Error::Parse(vec![Cow::Owned(format!("expected {} to be a collection, but found a collection key", collection))])),
                            None => return Err(Error::Parse(vec![Cow::Owned(format!("{} was never defined", collection))]))
                        };

                        var_counter += 1;

                        line!(w, indent, "var {}", key);
                        line!(w, indent, "for({} in {}) {{", key, collection_name);
                        indent += 1;
                        line!(w, indent, "if({}.hasOwnProperty({})) {{", collection_name, key);
                        indent += 1;
                        line!(w, indent, "var {} = {}[{}];", value, collection_name, key);

                        if let &Some(ref ty) = ty {
                            scopes.push(Some((element, value, ty, opt_key.as_ref().map(|k| (k, key)))));
                        } else {
                            return Err(Error::Parse(vec![Cow::Owned(format!("content type of {} could not be inferred (is it used anywhere?)", collection))]));
                        }
                    },
                    &Token::End => {
                        if let (Some(Some((ref text_var, ref mut state))), Some(tag)) = (text.pop(), tags.last()) {
                            try!(append_text(w, indent, text_var, state, tag));
                        }
                        indent -= 1;
                        line!(w, indent, "}}");
                        if let Some(Some(_)) = scopes.pop() {
                            indent -= 1;
                            line!(w, indent, "}}");
                        }
                    }
                }

                Ok(())
            }));
        } else {
            return Err(Error::Parse(vec![Cow::Borrowed("too many {{ end }}")]));
        }

        indent -= 1;

        line!(w, indent, "}};");
        Ok(())
    }

    fn build_module<W, F>(&self, w: &mut W, build_templates: F) -> Result<(), Error> where
        W: Write,
        F: FnOnce(&mut W, u8) -> Result<(), Error>
    {
        if let Some(ref namespace) = self.namespace {
            match self.variable_state {
                VarState::New => line!(w, 0, "var {} = {{}};", namespace),
                VarState::Uninited => line!(w, 0, "{} = {{}};", namespace),
                _ => {}
            }
        }

        build_templates(w, 0)
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

fn find_param<'a, 'b>(param: &str, params: &'a HashMap<String, ContentType>, scopes: &'b [Option<(&'a str, String, &'a ContentType, Option<(&'a String, String)>)>]) -> Option<(Option<&'b str>, Option<&'a ContentType>)> {
    for scope in scopes.iter().rev() {
        if let &Some((ref name, ref alias, ref ty, ref opt_key)) = scope {
            if *name == param {
                return Some((Some(alias), Some(ty)));
            } else if let &Some((ref key, ref alias)) = opt_key {
                if *key == param {
                    return Some((Some(alias), None))
                }
            }
        }
    }

    params.get(param).map(|t| (None, Some(t)))
}

fn append_text<W: Write>(w: &mut W, indent: u8, var: &str, state: &mut TextState, tag: &str) -> Result<(), Error> {
    if state.has_content() {
        line!(w, indent, "{}.appendChild(document.createTextNode({}));", tag, var);
        *state = TextState::Leftovers;
    }

    Ok(())
}

fn write_attribute<'a, W: Write>(
    w: &mut W,
    indent: u8,
    var: &str,
    content: &Content,
    new: bool,
    params: &'a HashMap<String, ContentType>,
    scopes: &[Option<(&'a str, String, &'a ContentType, Option<(&'a String, String)>)>]
) -> Result<(), Error> {
    match content {
        &Content::String(ref content) => {
            if new {
                line!(w, indent, "var {} = \"{}\";", var, content);
            } else if content.len() > 0 {
                line!(w, indent, "{} += \"{}\";", var, content);
            }
        },
        &Content::Placeholder(ref placeholder) => {
            match find_param(placeholder, params, &scopes) {
                Some((alias, Some(&ContentType::String(_)))) | Some((alias, Some(&ContentType::Bool))) | Some((alias, None)) => {
                    if new {
                        line!(w, indent, "var {} = \"\";", var);
                    }
                    match alias {
                        Some(alias) => {
                            line!(w, indent, "if({} !== null) {{", alias);
                            line!(w, indent + 1, "{} += {};", var, alias);
                        },
                        None => {
                            line!(w, indent, "if(this.{} !== null) {{", placeholder);
                            line!(w, indent + 1, "{} += this.{};", var, placeholder);
                        }
                    }
                    line!(w, indent, "}}");
                },
                Some((_, Some(&ContentType::Template(_)))) => {
                    return Err(Error::Parse(vec![Cow::Borrowed("templates are not supported in attributes")]));
                },
                Some((_, Some(&ContentType::Collection(_, _)))) => {
                    return Err(Error::Parse(vec![Cow::Borrowed("collections cannot be used as text")]));
                },
                None => return Err(Error::Parse(vec![Cow::Owned(format!("{} was never defined", placeholder))]))
            }
        }
    }

    Ok(())
}

fn write_text<'a, W: Write>(
    w: &mut W,
    indent: u8,
    var: &str,
    state: &mut TextState,
    content: &Content,
    parent: &str,
    params: &'a HashMap<String, ContentType>,
    scopes: &[Option<(&'a str, String, &'a ContentType, Option<(&'a String, String)>)>]
) -> Result<(), Error> {
    match content {
        &Content::String(ref content) => if content.len() > 0 {
            line!(w, indent, "{} += \"{}\";", var, content);
            *state = TextState::HasContent;
        },
        &Content::Placeholder(ref placeholder) => {
            match find_param(placeholder, params, &scopes) {
                Some((alias, Some(&ContentType::String(_)))) | Some((alias, Some(&ContentType::Bool))) | Some((alias, None)) => {
                    match alias {
                        Some(alias) => {
                            line!(w, indent, "if({} !== null) {{", alias);
                            line!(w, indent + 1, "{} += {};", var, alias);
                        },
                        None => {
                            line!(w, indent, "if(this.{} !== null) {{", placeholder);
                            line!(w, indent + 1, "{} += this.{};", var, placeholder);
                        }
                    }
                    line!(w, indent, "}}");
                    *state = TextState::HasContent;
                },
                Some((alias, Some(&ContentType::Template(_)))) => {
                    try!(append_text(w, indent, var, state, parent));
                    match alias {
                        Some(alias) => {
                            line!(w, indent, "if({} !== null) {{", alias);
                        },
                        None => {
                            line!(w, indent, "if(this.{} !== null) {{", placeholder);
                        }
                    }
                    match alias {
                        Some(alias) => {
                            line!(w, indent, "if({} !== null) {{", alias);
                            try!(write_indent(w, indent));
                            try!(write!(w, "{}", alias));
                        },
                        None => {
                            line!(w, indent, "if(this.{} !== null) {{", placeholder);
                            try!(write_indent(w, indent));
                            try!(write!(w, "this.{}", placeholder));
                        }
                    }
                    try!(write!(w, ".render_to({});", parent));
                    line!(w, indent, "}}");
                },
                Some((_, Some(&ContentType::Collection(_, _)))) => {
                    return Err(Error::Parse(vec![Cow::Borrowed("collections cannot be used as text")]));
                },
                None => return Err(Error::Parse(vec![Cow::Owned(format!("{} was never defined", placeholder))]))
            }
        }
    }

    Ok(())
}