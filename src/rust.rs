use std::io::{self, Write};
use std::collections::HashMap;
use std::default::Default;
use codegen::{Codegen, Logic, Token, Scope, ContentType, Content, write_indent};

///Visibility of modules and structures in Rust.
pub enum Visibility {
    Public,
    Private
}

///Code generator for Rust templates.
pub struct Rust<'a> {
    ///Generate a submodule. Defaults to none and should only be used when a
    ///whole module is generated.
    pub named_module: Option<(&'a str, Visibility)>,

    ///The template visibility. Defaults to public.
    pub visibility: Visibility
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
            visibility: Visibility::Public
        }
    }
}

impl<'a> Codegen for Rust<'a> {
    fn build_template<W: Write>(&self, w: &mut W, name: &str, mut indent: u8, params: &HashMap<String, ContentType>, tokens: &[Token]) -> io::Result<()> {
        let public = match (&self.visibility, &self.named_module) {
            (&Visibility::Public, _) => true,
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
                &Visibility::Public => line!(w, 0, "pub mod {} {{", module),
                &Visibility::Private => line!(w, 0, "mod {} {{", module)
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
