use std::io::{self, Write};
use std::fmt::Write as FmtWrite;
use std::collections::HashMap;
use std::default::Default;
use std::fmt;

use codegen::{Codegen, Logic, Token, Scope, ContentType, Content, write_indent};

#[derive(Debug)]
pub enum Error {
    UnknownType(String),
    UndefinedPlaceholder(String),
    CannotBeRendered(String),
    UnexpectedType(String, ContentType),

    Io(io::Error),
    Fmt(fmt::Error)
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Error::UnknownType(ref placeholder) => write!(f, "the type of '{}' was not fully inferred", placeholder),
            &Error::UndefinedPlaceholder(ref name) => write!(f, "the placeholder '{}' was not defined", name),
            &Error::CannotBeRendered(ref name) => write!(f, "the placeholder '{}' cannot be rendered as text", name),
            &Error::UnexpectedType(ref name, ref ty) => write!(f, "expected the placeholder '{}' to be of type '{}'", name, ty),

            &Error::Io(ref e) => e.fmt(f),
            &Error::Fmt(ref e) => e.fmt(f)
        }
    }
}

impl From<io::Error> for Error {
    fn from(error: io::Error) -> Error {
        Error::Io(error)
    }
}

impl From<fmt::Error> for Error {
    fn from(error: fmt::Error) -> Error {
        Error::Fmt(error)
    }
}

///Visibility of modules and structures in Rust.
pub enum Visibility {
    Public,
    Private
}

enum ParamTy {
    Param,
    Scope
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
                    Some(&ContentType::String(false)) | Some(&ContentType::Template(false)) | Some(&ContentType::Collection(_, false)) => try!(write!(w, "true")),
                    Some(&ContentType::String(true)) | Some(&ContentType::Template(true)) | Some(&ContentType::Collection(_, true)) => try!(write!(w, "self.{}.is_some()", val)),
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
    type Error = Error;
    
    fn build_template<W: Write>(&self, w: &mut W, name: &str, mut indent: u8, params: &HashMap<String, ContentType>, tokens: &[Token]) -> Result<(), Error> {
        let public = match (&self.visibility, &self.named_module) {
            (&Visibility::Public, _) => true,
            (_, &Some(_)) => true,
            _ => false
        };

        let mut string_buf = String::new();
        let mut fmt_args = vec![];
        let mut scopes = vec![];

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
            try!(write_indent(w, indent + 1));
            try!(write!(w, "pub {}: ", parameter));
            match write_ty(w, ty) {
                Err(Error::UnknownType(_)) => return Err(Error::UnknownType(parameter.clone())),
                r => try!(r)
            }
            try!(write!(w, ",\n"));
        }
        
        if params.len() > 0 {
            line!(w, indent, "}}");
        }

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
                &Token::SetDoctype(ref doctype) => {
                    string_buf.push_str("<!DOCTYPE");
                    if let Some(ref name) = doctype.name {
                        try!(write!(&mut string_buf, " {}", name));
                    }

                    if let Some(ref public_id) = doctype.public_id {
                        try!(write!(&mut string_buf, " PUBLIC \\\"{}\\\"", public_id));
                    } else if doctype.system_id.is_some() {
                        string_buf.push_str(" SYSTEM");
                    }

                    if let Some(ref system_id) = doctype.system_id {
                        try!(write!(&mut string_buf, " \\\"{}\\\"", system_id));
                    }
                    string_buf.push_str(">");
                },
                &Token::BeginTag(ref name) => try!(write!(&mut string_buf, "<{}", name.as_slice())),
                &Token::EndTag(_self_close) => try!(write!(&mut string_buf, ">")),
                &Token::CloseTag(ref name) => try!(write!(&mut string_buf, "</{}>", name.as_slice())),
                &Token::BeginAttribute(ref name, ref content) => match content {
                    &Content::String(ref content) => try!(write!(&mut string_buf, " {}=\\\"{}", name.as_slice(), content)),
                    &Content::Placeholder(ref placeholder) => {
                        match find_param(placeholder, params, &scopes) {
                            Some((param_ty, Some(&ContentType::String(false)))) | Some((param_ty, None)) => {
                                try!(write!(&mut string_buf, " {}=\\\"{{}}", name.as_slice()));
                                if let ParamTy::Param = param_ty {
                                    fmt_args.push(format!("self.{}", placeholder));
                                } else {
                                    fmt_args.push(placeholder.clone());
                                }
                            },
                            Some((param_ty, Some(&ContentType::String(true)))) => {
                                try!(write!(&mut string_buf, " {}=\\\"", name.as_slice()));
                                try!(try_write_and_clear_fmt(w, indent, &mut string_buf, &mut fmt_args));

                                if let ParamTy::Param = param_ty {
                                    line!(w, indent, "if let Some(val) = self.{} {{", placeholder);
                                } else {
                                    line!(w, indent, "if let Some(val) = {} {{", placeholder);
                                }

                                line!(w, indent + 1, "try!(write!(writer, \"{{}}\", val));");
                                line!(w, indent, "}}");
                            },
                            Some((param_ty, Some(&ContentType::Template(false)))) => {
                                try!(write!(&mut string_buf, " {}=\\\"", name.as_slice()));
                                try!(try_write_and_clear_fmt(w, indent, &mut string_buf, &mut fmt_args));

                                if let ParamTy::Param = param_ty {
                                    line!(w, indent, "try!(self.{}.render_to(writer));", placeholder);
                                } else {
                                    line!(w, indent, "try!({}.render_to(writer));", placeholder);
                                }
                            },
                            Some((param_ty, Some(&ContentType::Template(true)))) => {
                                try!(write!(&mut string_buf, " {}=\\\"", name.as_slice()));
                                try!(try_write_and_clear_fmt(w, indent, &mut string_buf, &mut fmt_args));

                                if let ParamTy::Param = param_ty {
                                    line!(w, indent, "if let Some(template) = self.{} {{", placeholder);
                                } else {
                                    line!(w, indent, "if let Some(template) = {} {{", placeholder);
                                }

                                line!(w, indent + 1, "try!(template.render_to(writer));");
                                line!(w, indent, "}}");
                            },
                            Some((_, Some(_ty))) => return Err(Error::CannotBeRendered(placeholder.clone())),
                            None => return Err(Error::UndefinedPlaceholder(placeholder.clone()))
                        }
                    }
                },
                &Token::AppendToAttribute(ref text) | &Token::Text(ref text) => match text {
                    &Content::String(ref content) => try!(write!(&mut string_buf, "{}", content)),
                    &Content::Placeholder(ref placeholder) => {
                        match find_param(placeholder, params, &scopes) {
                            Some((param_ty, Some(&ContentType::String(false)))) | Some((param_ty, None)) => {
                                try!(write!(&mut string_buf, "{{}}"));
                                if let ParamTy::Param = param_ty {
                                    fmt_args.push(format!("self.{}", placeholder));
                                } else {
                                    fmt_args.push(placeholder.clone());
                                }
                            },
                            Some((param_ty, Some(&ContentType::String(true)))) => {
                                try!(try_write_and_clear_fmt(w, indent, &mut string_buf, &mut fmt_args));

                                if let ParamTy::Param = param_ty {
                                    line!(w, indent, "if let Some(val) = self.{} {{", placeholder);
                                } else {
                                    line!(w, indent, "if let Some(val) = {} {{", placeholder);
                                }

                                line!(w, indent + 1, "try!(write!(writer, \"{{}}\", val));");
                                line!(w, indent, "}}");
                            },
                            Some((param_ty, Some(&ContentType::Template(false)))) => {
                                try!(try_write_and_clear_fmt(w, indent, &mut string_buf, &mut fmt_args));

                                if let ParamTy::Param = param_ty {
                                    line!(w, indent, "try!(self.{}.render_to(writer));", placeholder);
                                } else {
                                    line!(w, indent, "try!({}.render_to(writer));", placeholder);
                                }
                            },
                            Some((param_ty, Some(&ContentType::Template(true)))) => {
                                try!(try_write_and_clear_fmt(w, indent, &mut string_buf, &mut fmt_args));

                                if let ParamTy::Param = param_ty {
                                    line!(w, indent, "if let Some(template) = self.{} {{", placeholder);
                                } else {
                                    line!(w, indent, "if let Some(template) = {} {{", placeholder);
                                }

                                line!(w, indent + 1, "try!(template.render_to(writer));");
                                line!(w, indent, "}}");
                            },
                            Some((_, Some(_ty))) => return Err(Error::CannotBeRendered(placeholder.clone())),
                            None => return Err(Error::UndefinedPlaceholder(placeholder.clone()))
                        }
                    }
                },
                &Token::EndAttribute => try!(write!(&mut string_buf, "\\\"")),
                &Token::Scope(Scope::If(ref cond)) => {
                    scopes.push(None);
                    try!(try_write_and_clear_fmt(w, indent, &mut string_buf, &mut fmt_args));

                    try!(write_indent(w, indent));
                    try!(write!(w, "if "));
                    try!(self.eval_logic(w, true, &cond.flattened(), params));
                    try!(write!(w, " {{\n"));
                    indent += 1;
                },
                &Token::Scope(Scope::ForEach(ref collection, ref element, ref opt_key)) => {
                    try!(try_write_and_clear_fmt(w, indent, &mut string_buf, &mut fmt_args));

                    match find_param(collection, params, &scopes) {
                        Some((param_ty, content_ty)) => {
                            match content_ty {
                                Some(&ContentType::Collection(ref ty, optional)) => {
                                    try!(write_indent(w, indent));

                                    if let &Some(ref key) = opt_key {
                                        try!(write!(w, "for ({}, {})", key, element));
                                    } else {
                                        try!(write!(w, "for {}", element));
                                    }

                                    if let ParamTy::Param = param_ty {
                                        try!(write!(w, " in self.{}", collection));
                                    } else {
                                        try!(write!(w, " in {}", collection));
                                    }

                                    if optional {
                                        try!(write!(w, ".iter().flat_map(|c| c"));
                                    }

                                    if opt_key.is_some() {
                                        try!(write!(w, ".key_values()"));
                                    } else {
                                        try!(write!(w, ".values()"));
                                    }

                                    if optional {
                                        try!(write!(w, ") {{\n"));
                                    } else {
                                        try!(write!(w, " {{\n"));
                                    }

                                    if let &Some(ref ty) = ty {
                                        scopes.push(Some((element, ty, opt_key.as_ref())));
                                    } else {
                                        return Err(Error::UnknownType(collection.clone()))
                                    }
                                },
                                Some(_ty) => return Err(Error::UnexpectedType(collection.clone(), ContentType::Collection(None, false))),
                                None => return Err(Error::UnexpectedType(collection.clone(), ContentType::Collection(None, false)))
                            }
                        },
                        None => return Err(Error::UndefinedPlaceholder(collection.clone())),
                    }

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

fn write_ty<W: Write>(w: &mut W, ty: &ContentType) -> Result<(), Error> {
    if ty.is_optional() {
        try!(write!(w, "Option<"));
    }

    match ty {
        &ContentType::String(_) => try!(write!(w, "&'a ::std::fmt::Display")),
        &ContentType::Bool => try!(write!(w, "bool")),
        &ContentType::Template(_) => try!(write!(w, "&'a ::symbiosis_rust::Template")),
        &ContentType::Collection(Some(ref inner), _) => {
            try!(write!(w, "&'a ::symbiosis_rust::Collection<'a, "));
            try!(write_ty(w, inner));
            try!(write!(w, ">"));
        },
        &ContentType::Collection(None, _) => {
            return Err(Error::UnknownType("".into()))
        }
    }

    if ty.is_optional() {
        try!(write!(w, ">"));
    }

    Ok(())
}

fn find_param<'a>(param: &str, params: &'a HashMap<String, ContentType>, scopes: &[Option<(&str, &'a ContentType, Option<&String>)>]) -> Option<(ParamTy, Option<&'a ContentType>)> {
    for scope in scopes.iter().rev() {
        if let &Some((ref name, ref ty, ref opt_key)) = scope {
            if *name == param {
                return Some((ParamTy::Scope,Some(ty)));
            } else if let &Some(ref key) = opt_key {
                if *key == param {
                    return Some((ParamTy::Scope, None))
                }
            }
        }
    }

    params.get(param).map(|t| (ParamTy::Param,Some(t)))
}
