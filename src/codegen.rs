use std::io::{self, Write};
use std::collections::HashMap;
use std::default::Default;

use string_cache::atom::Atom;

///Write an indented line of code.
///
///Example: `line!(writer, indent_steps, "var {} = this.{};", var_name, field_name);`.
#[macro_export]
macro_rules! line {
    ($writer:ident, $indent:expr, $($format:tt)*) => (
        {
            try!($crate::codegen::write_indent($writer, $indent));
            try!(writeln!($writer, $($format)*));
        }
    )
}

///Code generators are used to generate template code in different languages
///and they have to implement this trait.
pub trait Codegen {
    ///Generate code for a single template.
    fn build_template<W: Write>(&self, w: &mut W, name: &str, indent: u8, params: &HashMap<String, ContentType>, tokens: &[Token]) -> io::Result<()>;

    ///Generate code for a module or a similar collection containing multiple templates.
    fn build_module<W, F>(&self, w: &mut W, build_templates: F) -> io::Result<()> where
        W: Write,
        F: FnOnce(&mut W, u8) -> io::Result<()>
    {
        build_templates(w, 0)
    }
}

///Tokens representing different parts of a template document.
pub enum Token {
    BeginTag(Atom),
    EndTag(bool),
    CloseTag(Atom),
    BeginAttribute(Atom, Content),
    AppendToAttribute(Content),
    EndAttribute,
    BeginText(Content),
    AppendToText(Content),
    EndText,
    Scope(Scope),
    End,
}

///Representation of supported scope types.
pub enum Scope {
    ///Everything within an `if` scope will be hidden if the provided logic
    ///expression is false.
    If(Logic)
}

///Logic expressions.
pub enum Logic {
    ///a and b and ...
    And(Vec<Logic>),
    ///a or b or ...
    Or(Vec<Logic>),
    ///The logical inverse of an expression.
    Not(Box<Logic>),
    ///A value from a template parameter.
    Value(String)
}

impl Logic {
    pub fn parameters<F: FnMut(&String)>(&self, f: &mut F) {
        match self {
            &Logic::And(ref conds) => for cond in conds {
                cond.parameters(f);
            },
            &Logic::Or(ref conds) => for cond in conds {
                cond.parameters(f);
            },
            &Logic::Not(ref cond) => cond.parameters(f),
            &Logic::Value(ref name) => f(name)
        }
    }

    pub fn flattened(&self) -> Logic {
        match self {
            &Logic::And(ref conds) if conds.len() == 1 => conds.first().unwrap().flattened(),
            &Logic::And(ref conds) => Logic::And(conds.iter().map(|c| c.flattened()).collect()),
            &Logic::Or(ref conds) if conds.len() == 1 => conds.first().unwrap().flattened(),
            &Logic::Or(ref conds) => Logic::Or(conds.iter().map(|c| c.flattened()).collect()),
            &Logic::Not(ref cond) => match &**cond {
                &Logic::Not(ref cond) => cond.flattened(),
                other => Logic::Not(Box::new(other.flattened()))
            },
            &Logic::Value(ref name) => Logic::Value(name.clone())
        }
    }
}

///Types of text content.
pub enum Content {
    ///A plain string.
    String(String),

    ///Use content from a parameter in a placeholder.
    Placeholder(String)
}

///Types of template parameter content.
pub enum ContentType {
    ///A plain string.
    String,
    ///An optionally defined string.
    OptionalString,
    ///A boolean value.
    Bool
}

///Represents the state of a JavaScript variable.
pub enum JsVarState {
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
    pub variable_state: JsVarState,

    ///The name of the namespace in which the template should be created, if
    ///any. Defaults to none.
    pub namespace: Option<&'a str>
}

impl<'a> JavaScript<'a> {
    fn eval_logic<W: Write>(&self, w: &mut W, first: bool, cond: &Logic, params: &HashMap<String, ContentType>) -> io::Result<()> {
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
            variable_state: JsVarState::New,
            namespace: None
        }
    }
}

impl<'a> Codegen for JavaScript<'a> {
    fn build_template<W: Write>(&self, w: &mut W, name: &str, mut indent: u8, params: &HashMap<String, ContentType>, tokens: &[Token]) -> io::Result<()> {
        if let Some(namespace) = self.namespace {
            line!(w, indent, "{}.{} = function() {{", namespace, name);
        } else {
            match self.variable_state {
                JsVarState::New => line!(w, indent, "var {} = function() {{", name),
                JsVarState::Inited | JsVarState::Uninited => line!(w, indent, "{} = function() {{", name),
            }
        }

        for (parameter, ty) in params {
            match ty {
                &ContentType::String => line!(w, indent + 1, "this.{} = \"\";", parameter),
                &ContentType::OptionalString => line!(w, indent + 1, "this.{} = null;", parameter),
                &ContentType::Bool => line!(w, indent + 1, "this.{} = false;", parameter),
            }
        }

        line!(w, indent, "}};");


        let mut var_counter = 0u32;
        let mut element_stack = vec![];
        let mut attribute_var = (Atom::from_slice(""), String::new());
        let mut text_var = String::new();

        if let Some(namespace) = self.namespace {
            line!(w, indent, "{}.{}.prototype.render_to = function(root) {{", namespace, name);
        } else {
            line!(w, indent, "{}.prototype.render_to = function(root) {{", name);
        }

        indent += 1;

        for token in tokens {
            match token {
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
                            line!(w, indent, "var {};", attribute_var.1);
                            line!(w, indent, "if(this.{} !== null) {{", placeholder);
                            line!(w, indent + 1, "{} = this.{};", attribute_var.1, placeholder);
                            line!(w, indent, "}} else {{");
                            line!(w, indent + 1, "{} = \"\";", attribute_var.1);
                            line!(w, indent, "}}");
                        }
                    }
                },
                &Token::AppendToAttribute(ref content) => {
                    match content {
                        &Content::String(ref content) => line!(w, indent, "{} += \"{}\";", attribute_var.1, content),
                        &Content::Placeholder(ref placeholder) => {
                            line!(w, indent, "if(this.{} !== null) {{", placeholder);
                            line!(w, indent + 1, "{} += this.{};", attribute_var.1, placeholder);
                            line!(w, indent, "}}");
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
                            line!(w, indent, "var {};", text_var);
                            line!(w, indent, "if(this.{} !== null) {{", placeholder);
                            line!(w, indent + 1, "{} = this.{};", text_var, placeholder);
                            line!(w, indent, "}} else {{");
                            line!(w, indent + 1, "{} = \"\";", text_var);
                            line!(w, indent, "}}");
                        }
                    }
                },
                &Token::AppendToText(ref content) => {
                    match content {
                        &Content::String(ref content) => line!(w, indent, "{} += \"{}\";", text_var, content),
                        &Content::Placeholder(ref placeholder) => {
                            line!(w, indent, "if(this.{} !== null) {{", placeholder);
                            line!(w, indent + 1, "{} += this.{};", text_var, placeholder);
                            line!(w, indent, "}}");
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
                },
                &Token::End => {
                    indent -= 1;
                    line!(w, indent, "}}");
                }
            }
        }

        indent -= 1;

        line!(w, indent, "}};");
        Ok(())
    }

    fn build_module<W, F>(&self, w: &mut W, build_templates: F) -> io::Result<()> where
        W: Write,
        F: FnOnce(&mut W, u8) -> io::Result<()>
    {
        if let Some(ref namespace) = self.namespace {
            match self.variable_state {
                JsVarState::New => line!(w, 0, "var {} = {{}};", namespace),
                JsVarState::Uninited => line!(w, 0, "{} = {{}};", namespace),
                _ => {}
            }
        }

        build_templates(w, 0)
    }
}

///Visibility of modules and structures in Rust.
pub enum RustVisibility {
    Public,
    Private
}

///Code generator for Rust templates.
pub struct Rust<'a> {
    ///Generate a submodule. Defaults to none and should only be used when a
    ///whole module is generated.
    pub named_module: Option<(&'a str, RustVisibility)>,

    ///The template visibility. Defaults to public.
    pub visibility: RustVisibility
}

impl<'a> Rust<'a> {
    fn eval_logic<W: Write>(&self, w: &mut W, first: bool, cond: &Logic, params: &HashMap<String, ContentType>) -> io::Result<()> {
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
                match params.get(val) {
                    Some(&ContentType::String) => try!(write!(w, "true")),
                    Some(&ContentType::OptionalString) => try!(write!(w, "self.{}.is_some()", val)),
                    Some(&ContentType::Bool) => try!(write!(w, "self.{}", val)),
                    None => {}
                }
            }
        }

        Ok(())
    }
}

impl<'a> Default for Rust<'a> {
    fn default() -> Rust<'a> {
        Rust {
            named_module: None,
            visibility: RustVisibility::Public
        }
    }
}

impl<'a> Codegen for Rust<'a> {
    fn build_template<W: Write>(&self, w: &mut W, name: &str, mut indent: u8, params: &HashMap<String, ContentType>, tokens: &[Token]) -> io::Result<()> {
        let public = match (&self.visibility, &self.named_module) {
            (&RustVisibility::Public, _) => true,
            (_, &Some(_)) => true,
            _ => false
        };

        if public {
            if params.len() > 0 {
                line!(w, indent, "pub struct {}<'a> {{", name);
            } else {
                line!(w, indent, "pub struct {};", name);
            }
        } else {
            if params.len() > 0 {
                line!(w, indent, "struct {}<'a> {{", name);
            } else {
                line!(w, indent, "struct {};", name);
            }
        }

        for (parameter, ty) in params {
            match ty {
                &ContentType::String => line!(w, indent + 1, "pub {}: &'a str,", parameter),
                &ContentType::OptionalString => line!(w, indent + 1, "pub {}: Option<&'a str>,", parameter),
                &ContentType::Bool => line!(w, indent + 1, "pub {}: bool,", parameter),
            }
        }
        
        line!(w, indent, "}}");

        if params.len() > 0 {
            line!(w, indent, "impl<'a> {}<'a> {{", name);
        } else {
            line!(w, indent, "impl {} {{", name);
        }
        indent += 1;

        line!(w, indent, "pub fn render_to<W: ::std::io::Write>(&self, writer: &mut W) -> ::std::io::Result<()> {{");
        indent += 1;

        for token in tokens {
            match token {
                &Token::BeginTag(ref name) => line!(w, indent, "try!(write!(writer, \"<{}\"));", name.as_slice()),
                &Token::EndTag(_self_close) => line!(w, indent, "try!(write!(writer, \">\"));"),
                &Token::CloseTag(ref name) => line!(w, indent, "try!(write!(writer, \"</{}>\"));", name.as_slice()),
                &Token::BeginAttribute(ref name, ref content) => match content {
                    &Content::String(ref content) => line!(w, indent, "try!(write!(writer, \" {}=\\\"{}\"));", name.as_slice(), content),
                    &Content::Placeholder(ref placeholder) => {
                        match params.get(placeholder) {
                            Some(&ContentType::String) => line!(w, indent, "try!(write!(writer, \" {}=\\\"{{}}\", self.{}));", name.as_slice(), placeholder),
                            Some(&ContentType::OptionalString) => {
                                line!(w, indent, "try!(write!(writer, \" {}=\\\"\"));", name.as_slice());
                                line!(w, indent, "if let Some(val) = self.{} {{", placeholder);
                                line!(w, indent + 1, "try!(write!(writer, \"{{}}\", val));");
                                line!(w, indent, "}}");
                            },
                            _ => {}
                        }
                    }
                },
                &Token::AppendToAttribute(ref content) => match content {
                    &Content::String(ref content) => line!(w, indent, "try!(write!(writer, \"{}\"));", content),
                    &Content::Placeholder(ref placeholder) => {
                        match params.get(placeholder) {
                            Some(&ContentType::String) => line!(w, indent, "try!(write!(writer, \"{{}}\", self.{}));", placeholder),
                            Some(&ContentType::OptionalString) => {
                                line!(w, indent, "if let Some(val) = self.{} {{", placeholder);
                                line!(w, indent + 1, "try!(write!(writer, \"{{}}\", val));");
                                line!(w, indent, "}}");
                            },
                            _ => {}
                        }
                    }
                },
                &Token::EndAttribute => line!(w, indent, "try!(write!(writer, \"\\\"\"));"),
                &Token::BeginText(ref text) | &Token::AppendToText(ref text) => match text {
                    &Content::String(ref text) => line!(w, indent, "try!(write!(writer, \"{}\"));", text),
                    &Content::Placeholder(ref placeholder) => {
                        match params.get(placeholder) {
                            Some(&ContentType::String) => line!(w, indent, "try!(write!(writer, \"{{}}\", self.{}));", placeholder),
                            Some(&ContentType::OptionalString) => {
                                line!(w, indent, "if let Some(val) = self.{} {{", placeholder);
                                line!(w, indent + 1, "try!(write!(writer, \"{{}}\", val));");
                                line!(w, indent, "}}");
                            },
                            _ => {}
                        }
                    },
                },
                &Token::EndText => {},
                &Token::Scope(Scope::If(ref cond)) => {
                    try!(write_indent(w, indent));
                    try!(write!(w, "if "));
                    try!(self.eval_logic(w, true, &cond.flattened(), params));
                    try!(write!(w, " {{\n"));
                    indent += 1;
                },
                &Token::End => {
                    indent -= 1;
                    line!(w, indent, "}}");
                }
            }
        }

        line!(w, indent, "Ok(())");
        indent -= 1;

        line!(w, indent, "}}");
        indent -= 1;

        line!(w, indent, "}}");
        Ok(())
    }

    fn build_module<W, F>(&self, w: &mut W, build_templates: F) -> io::Result<()> where
        W: Write,
        F: FnOnce(&mut W, u8) -> io::Result<()>
    {
        let indent = if let Some((ref module, ref visibility)) = self.named_module {
            match visibility {
                &RustVisibility::Public => line!(w, 0, "pub mod {} {{", module),
                &RustVisibility::Private => line!(w, 0, "mod {} {{", module)
            }
            1
        } else {
            0
        };

        try!(build_templates(w, indent));

        if self.named_module.is_some() {
            line!(w, 0, "}}");
        }

        Ok(())
    }
}

///Write `4 * steps` spaces.
#[inline]
pub fn write_indent<W: Write>(writer: &mut W, steps: u8) -> io::Result<()> {
    for _ in 0..steps {
        try!(write!(writer, "    "));
    }
    Ok(())
}

fn to_valid_ident(name: &str) -> String {
    name.chars().map(|c| if !c.is_alphanumeric() { '_' } else { c }).collect()
}