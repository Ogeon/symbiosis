use std::io::Write;
use std::fmt::Write as FmtWrite;
use std::collections::HashMap;
use std::default::Default;
use std::borrow::Cow;

use codegen::{Codegen, Logic, Token, Scope, ContentType, Content, write_indent};

use Error;

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
                match params.get(val) {
                    Some(&ContentType::String) | Some(&ContentType::Template) => try!(write!(w, "true")),
                    Some(&ContentType::OptionalString) | Some(&ContentType::OptionalTemplate) => try!(write!(w, "self.{}.is_some()", val)),
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
    fn build_template<W: Write>(&self, w: &mut W, name: &str, mut indent: u8, params: &HashMap<String, ContentType>, tokens: &[Token]) -> Result<(), Error> {
        let public = match (&self.visibility, &self.named_module) {
            (&Visibility::Public, _) => true,
            (_, &Some(_)) => true,
            _ => false
        };

        let mut string_buf = String::new();
        let mut fmt_args = vec![];

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
                &ContentType::Template => line!(w, indent + 1, "pub {}: &'a ::symbiosis_rust::Template,", parameter),
                &ContentType::OptionalTemplate => line!(w, indent + 1, "pub {}: Option<&'a ::symbiosis_rust::Template>,", parameter),
            }
        }
        
        line!(w, indent, "}}");

        if params.len() > 0 {
            line!(w, indent, "impl<'a> ::symbiosis_rust::Template for {}<'a> {{", name);
        } else {
            line!(w, indent, "impl ::symbiosis_rust::Template for {} {{", name);
        }
        indent += 1;

        line!(w, indent, "fn render_to(&self, writer: &mut ::std::io::Write) -> ::std::io::Result<()> {{");
        indent += 1;

        for token in tokens {
            match token {
                &Token::BeginTag(ref name) => try!(write!(&mut string_buf, "<{}", name.as_slice())),
                &Token::EndTag(_self_close) => try!(write!(&mut string_buf, ">")),
                &Token::CloseTag(ref name) => try!(write!(&mut string_buf, "</{}>", name.as_slice())),
                &Token::BeginAttribute(ref name, ref content) => match content {
                    &Content::String(ref content) => try!(write!(&mut string_buf, " {}=\\\"{}", name.as_slice(), content)),
                    &Content::Placeholder(ref placeholder) => {

                        match params.get(placeholder) {
                            Some(&ContentType::String) => {
                                try!(write!(&mut string_buf, " {}=\\\"{{}}", name.as_slice()));
                                fmt_args.push(format!("self.{}", placeholder));
                            },
                            Some(&ContentType::OptionalString) => {
                                try!(write!(&mut string_buf, " {}=\\\"", name.as_slice()));
                                try!(try_write_and_clear_fmt(w, indent, &mut string_buf, &mut fmt_args));

                                line!(w, indent, "if let Some(val) = self.{} {{", placeholder);
                                line!(w, indent + 1, "try!(write!(writer, \"{{}}\", val));");
                                line!(w, indent, "}}");
                            },
                            Some(&ContentType::Template) => {
                                try!(write!(&mut string_buf, " {}=\\\"", name.as_slice()));
                                try!(try_write_and_clear_fmt(w, indent, &mut string_buf, &mut fmt_args));

                                line!(w, indent, "try!(self.{}.render_to(writer));", placeholder);
                            },
                            Some(&ContentType::OptionalTemplate) => {
                                try!(write!(&mut string_buf, " {}=\\\"", name.as_slice()));
                                try!(try_write_and_clear_fmt(w, indent, &mut string_buf, &mut fmt_args));

                                line!(w, indent, "if let Some(template) = self.{} {{", placeholder);
                                line!(w, indent + 1, "try!(template.render_to(writer));");
                                line!(w, indent, "}}");
                            },
                            Some(&ContentType::Bool) => return Err(Error::Parse(vec![Cow::Borrowed("boolean values cannot be rendered as text")])),
                            None => {}
                        }
                    }
                },
                &Token::AppendToAttribute(ref text) | &Token::BeginText(ref text) | &Token::AppendToText(ref text) => match text {
                    &Content::String(ref content) => try!(write!(&mut string_buf, "{}", content)),
                    &Content::Placeholder(ref placeholder) => {
                        match params.get(placeholder) {
                            Some(&ContentType::String) => {
                                try!(write!(&mut string_buf, "{{}}"));
                                fmt_args.push(format!("self.{}", placeholder));
                            },
                            Some(&ContentType::OptionalString) => {
                                try!(try_write_and_clear_fmt(w, indent, &mut string_buf, &mut fmt_args));

                                line!(w, indent, "if let Some(val) = self.{} {{", placeholder);
                                line!(w, indent + 1, "try!(write!(writer, \"{{}}\", val));");
                                line!(w, indent, "}}");
                            },
                            Some(&ContentType::Template) => {
                                try!(try_write_and_clear_fmt(w, indent, &mut string_buf, &mut fmt_args));

                                line!(w, indent, "try!(self.{}.render_to(writer));", placeholder);
                            },
                            Some(&ContentType::OptionalTemplate) => {
                                try!(try_write_and_clear_fmt(w, indent, &mut string_buf, &mut fmt_args));

                                line!(w, indent, "if let Some(template) = self.{} {{", placeholder);
                                line!(w, indent + 1, "try!(template.render_to(writer));");
                                line!(w, indent, "}}");
                            },
                            Some(&ContentType::Bool) => return Err(Error::Parse(vec![Cow::Borrowed("boolean values cannot be rendered as text")])),
                            None => {}
                        }
                    }
                },
                &Token::EndAttribute => try!(write!(&mut string_buf, "\\\"")),
                &Token::EndText => {},
                &Token::Scope(Scope::If(ref cond)) => {
                    try!(try_write_and_clear_fmt(w, indent, &mut string_buf, &mut fmt_args));

                    try!(write_indent(w, indent));
                    try!(write!(w, "if "));
                    try!(self.eval_logic(w, true, &cond.flattened(), params));
                    try!(write!(w, " {{\n"));
                    indent += 1;
                },
                &Token::End => {
                    try!(try_write_and_clear_fmt(w, indent, &mut string_buf, &mut fmt_args));

                    indent -= 1;
                    line!(w, indent, "}}");
                }
            }
        }

        try!(write_and_clear_fmt(w, indent, &mut string_buf, &mut fmt_args));
        indent -= 1;

        line!(w, indent, "}}");
        indent -= 1;

        line!(w, indent, "}}");
        Ok(())
    }

    fn build_module<W, F>(&self, w: &mut W, build_templates: F) -> Result<(), Error> where
        W: Write,
        F: FnOnce(&mut W, u8) -> Result<(), Error>
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

fn try_write_and_clear_fmt<W: Write>(w: &mut W, indent: u8, buf: &mut String, args: &mut Vec<String>) -> Result<(), Error> {
    if buf.len() > 0 {
        if args.len() == 0 {
            line!(w, indent, "try!(write!(writer, \"{}\"));", buf);
        } else {
            line!(w, indent, "try!(write!(writer, \"{}\", {}));", buf, args.connect(", "));
        }
        buf.clear();
        args.clear();
    }

    Ok(())
}

fn write_and_clear_fmt<W: Write>(w: &mut W, indent: u8, buf: &mut String, args: &mut Vec<String>) -> Result<(), Error> {
    if buf.len() > 0 {
        if args.len() == 0 {
            line!(w, indent, "write!(writer, \"{}\")", buf);
        } else {
            line!(w, indent, "write!(writer, \"{}\", {})", buf, args.connect(", "));
        }
        buf.clear();
        args.clear();
    }

    Ok(())
}
