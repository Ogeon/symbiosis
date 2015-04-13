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


        let mut var_counter = 0u32;
        let mut element_stack = vec![];
        let mut attribute_var = (Atom::from_slice(""), String::new());
        let mut text_var = String::new();
        let mut scopes = vec![];

        if let Some(namespace) = self.namespace {
            line!(w, indent, "{}.{}.prototype.render_to = function(root) {{", namespace, name);
        } else {
            line!(w, indent, "{}.prototype.render_to = function(root) {{", name);
        }

        indent += 1;

        for token in tokens {
            match token {
                &Token::SetDoctype(_) => return Err(Error::Parse(vec![Cow::Borrowed("doctype declarations are currently not supported")])),
                &Token::BeginTag(ref name) => {
                    let var = format!("tag_{}_{}", to_valid_ident(name.as_slice()), var_counter);
                    var_counter += 1;
                    line!(w, indent, "var {} = document.createElement(\"{}\");", var, name.as_slice());
                    element_stack.push(var);
                },
                &Token::EndTag(self_close) => if self_close {
                    if let Some(child) = element_stack.pop() {
                        match element_stack.last() {
                            Some(parent) => line!(w, indent, "{}.appendChild({});", parent, child),
                            None => line!(w, indent, "root.appendChild({});", child)
                        }
                    }
                },
                &Token::CloseTag(_) => {
                    if let Some(child) = element_stack.pop() {
                        match element_stack.last() {
                            Some(parent) => line!(w, indent, "{}.appendChild({});", parent, child),
                            None => line!(w, indent, "root.appendChild({});", child)
                        }
                    }
                },
                &Token::BeginAttribute(ref name, ref content) => {
                    attribute_var = (name.clone(), format!("attr_{}_{}", to_valid_ident(name.as_slice()), var_counter));
                    var_counter += 1;
                    match content {
                        &Content::String(ref content) => line!(w, indent, "var {} = \"{}\";", attribute_var.1, content),
                        &Content::Placeholder(ref placeholder) => {
                            match find_param(placeholder, params, &scopes) {
                                Some((alias, Some(&ContentType::String(_)))) | Some((alias, Some(&ContentType::Bool))) | Some((alias, None)) => {
                                    line!(w, indent, "var {};", attribute_var.1);
                                    match alias {
                                        Some(alias) => {
                                            line!(w, indent, "if({} !== null) {{", alias);
                                            line!(w, indent + 1, "{} = {};", attribute_var.1, alias);
                                        },
                                        None => {
                                            line!(w, indent, "if(this.{} !== null) {{", placeholder);
                                            line!(w, indent + 1, "{} = this.{};", attribute_var.1, placeholder);
                                        }
                                    }
                                    line!(w, indent, "}} else {{");
                                    line!(w, indent + 1, "{} = \"\";", attribute_var.1);
                                    line!(w, indent, "}}");
                                },
                                Some((_, Some(&ContentType::Template(_)))) => {
                                    return Err(Error::Parse(vec![Cow::Borrowed("templates are not supported in attributes")]));
                                },
                                Some((_, Some(&ContentType::Collection(_, _)))) => {
                                    return Err(Error::Parse(vec![Cow::Borrowed("collections cannot be used as text")]));
                                },
                                None => {}
                            }
                        }
                    }
                },
                &Token::AppendToAttribute(ref content) => {
                    match content {
                        &Content::String(ref content) => line!(w, indent, "{} += \"{}\";", attribute_var.1, content),
                        &Content::Placeholder(ref placeholder) => {
                            match find_param(placeholder, params, &scopes) {
                                Some((alias, Some(&ContentType::String(_)))) | Some((alias, Some(&ContentType::Bool))) | Some((alias, None)) => {
                                    match alias {
                                        Some(alias) => {
                                            line!(w, indent, "if({} !== null) {{", alias);
                                            line!(w, indent + 1, "{} += {};", attribute_var.1, alias);
                                        },
                                        None => {
                                            line!(w, indent, "if(this.{} !== null) {{", placeholder);
                                            line!(w, indent + 1, "{} += this.{};", attribute_var.1, placeholder);
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
                                None => {}
                            }
                        }
                    }
                },
                &Token::EndAttribute => {
                    if let Some(element) = element_stack.last() {
                        line!(w, indent, "{}.setAttribute(\"{}\", {});", element, attribute_var.0.as_slice(), attribute_var.1);
                    }
                },
                &Token::BeginText(ref content) => {
                    text_var = format!("text_{}", var_counter);
                    var_counter += 1;
                    match content {
                        &Content::String(ref content) => line!(w, indent, "var {} = \"{}\";", text_var, content),
                        &Content::Placeholder(ref placeholder) => {
                            match find_param(placeholder, params, &scopes) {
                                Some((alias, Some(&ContentType::String(_)))) | Some((alias, Some(&ContentType::Bool))) | Some((alias, None)) => {
                                    line!(w, indent, "var {};", text_var);
                                    match alias {
                                        Some(alias) => {
                                            line!(w, indent, "if({} !== null) {{", alias);
                                            line!(w, indent + 1, "{} = {};", text_var, alias);
                                        },
                                        None => {
                                            line!(w, indent, "if(this.{} !== null) {{", placeholder);
                                            line!(w, indent + 1, "{} = this.{};", text_var, placeholder);
                                        }
                                    }
                                    line!(w, indent, "}} else {{");
                                    line!(w, indent + 1, "{} = \"\";", text_var);
                                    line!(w, indent, "}}");
                                },
                                Some((alias, Some(&ContentType::Template(_)))) => {
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
                                    if let Some(element) = element_stack.last() {
                                        try!(write!(w, ".render_to({});", element));
                                    } else {
                                        try!(write!(w, ".render_to(root);"));
                                    }
                                    line!(w, indent, "}}");
                                    line!(w, indent, "var {};", text_var);
                                },
                                Some((_, Some(&ContentType::Collection(_, _)))) => {
                                    return Err(Error::Parse(vec![Cow::Borrowed("collections cannot be used as text")]));
                                },
                                None => {}
                            }
                        }
                    }
                },
                &Token::AppendToText(ref content) => {
                    match content {
                        &Content::String(ref content) => line!(w, indent, "{} += \"{}\";", text_var, content),
                        &Content::Placeholder(ref placeholder) => {
                            match find_param(placeholder, params, &scopes) {
                                Some((alias, Some(&ContentType::String(_)))) | Some((alias, Some(&ContentType::Bool))) | Some((alias, None)) => {
                                    match alias {
                                        Some(alias) => {
                                            line!(w, indent, "if({} !== null) {{", alias);
                                            line!(w, indent + 1, "{} += {};", text_var, alias);
                                        },
                                        None => {
                                            line!(w, indent, "if(this.{} !== null) {{", placeholder);
                                            line!(w, indent + 1, "{} += this.{};", text_var, placeholder);
                                        }
                                    }
                                    line!(w, indent, "}}");
                                },
                                Some((alias, Some(&ContentType::Template(_)))) => {
                                    match alias {
                                        Some(alias) => {
                                            line!(w, indent, "if({} !== null) {{", alias);
                                        },
                                        None => {
                                            line!(w, indent, "if(this.{} !== null) {{", placeholder);
                                        }
                                    }
                                    if let Some(element) = element_stack.last() {
                                        line!(w, indent + 1, "{}.appendChild(document.createTextNode({}));", element, text_var);
                                    } else {
                                        line!(w, indent + 1, "root.appendChild(document.createTextNode({}));", text_var);
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
                                    if let Some(element) = element_stack.last() {
                                        try!(write!(w, ".render_to({});", element));
                                    } else {
                                        try!(write!(w, ".render_to(root);"));
                                    }
                                    line!(w, indent + 1, "{} = \"\";", text_var);
                                    line!(w, indent, "}}");
                                },
                                Some((_, Some(&ContentType::Collection(_, _)))) => {
                                    return Err(Error::Parse(vec![Cow::Borrowed("collections cannot be used as text")]));
                                },
                                None => {}
                            }
                        }
                    }
                },
                &Token::EndText => {
                    if let Some(element) = element_stack.last() {
                        line!(w, indent, "{}.appendChild(document.createTextNode({}));", element, text_var);
                    } else {
                        line!(w, indent, "root.appendChild(document.createTextNode({}));", text_var);
                    }
                }
                &Token::Scope(Scope::If(ref cond)) => {
                    try!(write_indent(w, indent));
                    try!(write!(w, "if("));
                    try!(self.eval_logic(w, true, &cond.flattened(), params));
                    try!(write!(w, ") {{\n"));
                    indent += 1;
                    scopes.push(None);
                },
                &Token::Scope(Scope::ForEach(ref collection, ref element, ref opt_key)) => {
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
                    indent -= 1;
                    line!(w, indent, "}}");
                    if let Some(Some(_)) = scopes.pop() {
                        indent -= 1;
                        line!(w, indent, "}}");
                    }
                }
            }
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