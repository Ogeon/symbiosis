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
use html5ever::tokenizer::{Tokenizer, TokenSink, Token, Tag, TagKind};

use string_cache::atom::Atom;

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
    var_counter: u32,
    element_stack: Vec<(String, Atom)>,
    rust_render: String,
    js_render: String
}

impl Template {
    fn new() -> Template {
        Template {
            parameters: HashSet::new(),
            var_counter: 0,
            element_stack: vec![],
            rust_render: String::new(),
            js_render: String::new()
        }
    }

    fn open_tag(&mut self, name: Atom, attributes: Vec<Attribute>, self_closing: bool) {
        let var = format!("{}_{}", to_valid_ident(name.as_slice()), self.var_counter);
        self.var_counter += 1;

        self.js_render.push_str(&format!("var {} = document.createElement(\"{}\");\n", var, name.as_slice()));
        self.rust_render.push_str(&format!("try!(write!(writer, \"<{}\"));\n", name.as_slice()));

        for attribute in attributes {
            self.js_render.push_str(&format!("{}.setAttribute(\"{}\", \"{}\"):\n", var, attribute.name.local.as_slice(), attribute.value));
            self.rust_render.push_str(&format!("try!(write!(writer, \" {}=\\\"{}\\\"\"));\n", attribute.name.local.as_slice(), attribute.value));
        }

        self.rust_render.push_str("try!(write!(writer, \">\"));\n");

        if self_closing || is_void(name.as_slice()) {
            self.append_child(&var);
        } else {
            self.element_stack.push((var, name));
        }
    }

    fn add_text(&mut self, text: String) {
        if let Some(&(ref element, _)) = self.element_stack.last() {
            self.js_render.push_str(&format!("{}.appendChild(document.createTextNode(\"{}\"));\n", element, text));
        } else {
            self.js_render.push_str(&format!("root.appendChild(document.createTextNode(\"{}\"));\n", text));
        }
        self.rust_render.push_str(&format!("try!(write!(writer, \"{}\"));\n", text));
    }

    fn close_tag(&mut self) {
        if let Some((element, tag)) = self.element_stack.pop() {
            self.append_child(&element);
            self.rust_render.push_str(&format!("try!(write!(writer, \"</{}>\"));\n", tag.as_slice()));
        }
    }

    fn append_child(&mut self, child: &str) {
        match self.element_stack.last() {
            Some(&(ref parent, _)) => {
                self.js_render.push_str(&format!("{}.appendChild({});\n", parent, child));
            },
            None => {
                self.js_render.push_str(&format!("root.appendChild({});\n", child));
            }
        }
    }

    ///Write a Rust template and define it as `name`. The template will be
    ///prefixed with `pub` if `public` is true.
    pub fn emit_rust<W: Write>(&self, public: bool, name: &str, writer: &mut W) -> io::Result<()> {
        if public {
            try!(write!(writer, "pub "));
        }

        if self.parameters.len() > 0 {
            try!(write!(writer, "struct {}<'a> {{\n", name));

            for parameter in &self.parameters {
                try!(write!(writer, "    pub {}: Option<&'a str>,\n", parameter));
            }

            try!(write!(writer, "}}\n\n"));
            try!(write!(writer, "impl<'a> {}<'a> {{\n", name));
        } else {
            try!(write!(writer, "struct {};\n\n", name));
            try!(write!(writer, "impl {} {{\n", name));
        }

        try!(write!(writer, "    pub fn render_to<W: ::std::io::Write>(&self, writer: &mut W) -> ::std::io::Result<()> {{\n"));
        try!(writer.write_all(self.rust_render.as_bytes()));
        try!(write!(writer, "    Ok(())\n"));
        try!(write!(writer, "    }}\n\n"));
        write!(writer, "}}\n\n")
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

        try!(write!(writer, "}}\n\n"));
        try!(write!(writer, "{}.prototype.render_to = function(root) {{\n", name));
        try!(writer.write_all(self.js_render.as_bytes()));
        write!(writer, "}}\n\n")
    }
}

impl TokenSink for Template {
    fn process_token(&mut self, token: Token) {
        match token {
            Token::TagToken(Tag {
                kind: TagKind::StartTag,
                name,
                attrs,
                self_closing
            }) => self.open_tag(name, attrs, self_closing),
            Token::TagToken(Tag {
                kind: TagKind::EndTag,
                //name,
                ..
            }) => self.close_tag(),
            Token::CharacterTokens(text) => self.add_text(text),
            _ => {}
        }
    }
}

fn to_valid_ident(name: &str) -> String {
    name.chars().map(|c| if !c.is_alphanumeric() { '_' } else { c }).collect()
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