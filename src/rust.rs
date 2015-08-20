use std::io::{self, Write};
use std::fmt::Write as FmtWrite;
use std::collections::HashMap;
use std::default::Default;
use std::fmt;

use tendril::StrTendril;

use codegen::{Codegen, Writer, Line, Logic, Token, Scope, ContentType, Content};

macro_rules! try_w_s {
    ($w: expr, $fmt: tt, $($arg: expr),*) => (try_w!($w, $fmt, $(Sanitized($arg)),*))
}

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
    fn eval_logic<W: Write>(&self, w: &mut Line<W>, first: bool, cond: &Logic, params: &HashMap<StrTendril, ContentType>) -> Result<(), Error> {
        match cond {
            &Logic::And(ref conds) => {
                if !first {
                    try_w!(w, "(");
                }
                for (i, cond) in conds.iter().enumerate() {
                    if i > 0 {
                        try_w!(w, " && ");
                    }
                    try!(self.eval_logic(w, false, cond, params));
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
                    try!(self.eval_logic(w, false, cond, params));
                }
                if !first {
                    try_w!(w, ")");
                }
            },
            &Logic::Not(ref cond) => {
                try_w!(w, "!(");
                try!(self.eval_logic(w, false, cond, params));
                try_w!(w, ")");
            },
            &Logic::Value(ref val) => {
                match params.get(val) {
                    Some(&ContentType::String(false)) | Some(&ContentType::Template(false)) | Some(&ContentType::Collection(_, false)) => try_w!(w, "true"),
                    Some(&ContentType::String(true)) | Some(&ContentType::Template(true)) | Some(&ContentType::Collection(_, true)) => try_w!(w, "self.{}.is_some()", val),
                    Some(&ContentType::Bool) => try_w!(w, "self.{}", val),
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

    fn init_writer<'w, W: Write>(&self, w: &'w mut W) -> Writer<'w, W> {
        Writer::new(w, "    ")
    }
    
    fn build_template<W: Write>(&self, w: &mut Writer<W>, name: &str, params: &HashMap<StrTendril, ContentType>, tokens: &[Token]) -> Result<(), Error> {
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
                try_w!(w, "pub struct {}<'a> {{", name);
            } else {
                try_w!(w, "pub struct {};", name);
            }
        } else {
            if params.len() > 0 {
                try_w!(w, "struct {}<'a> {{", name);
            } else {
                try_w!(w, "struct {};", name);
            }
        }

        {
            let mut block = w.block();
            for (parameter, ty) in params {
                let mut line = block.begin_line();
                try_w!(line, "pub {}: ", parameter);
                match write_ty(&mut line, ty) {
                    Err(Error::UnknownType(_)) => return Err(Error::UnknownType(parameter.into())),
                    r => try!(r)
                }
                try_w!(line, ",");
            }
        }
        
        if params.len() > 0 {
            try_w!(w, "}}");
        }

        if params.len() > 0 {
            try_w!(w, "impl<'a> ::symbiosis_rust::Template for {}<'a> {{", name);
        } else {
            try_w!(w, "impl ::symbiosis_rust::Template for {} {{", name);
        }

        w.indent();

        try_w!(w, "fn render_to(&self, writer: &mut ::std::io::Write) -> ::std::io::Result<()> {{");

        {
            let mut func = w.block();

            for token in tokens {
                match token {
                    &Token::SetDoctype(ref doctype) => {
                        string_buf.push_str("<!DOCTYPE");
                        if let Some(ref name) = doctype.name {
                            try_w_s!(string_buf, " {}", name);
                        }

                        if let Some(ref public_id) = doctype.public_id {
                            try_w_s!(string_buf, " PUBLIC \\\"{}\\\"", public_id);
                        } else if doctype.system_id.is_some() {
                            string_buf.push_str(" SYSTEM");
                        }

                        if let Some(ref system_id) = doctype.system_id {
                            try_w_s!(string_buf, " \\\"{}\\\"", system_id);
                        }
                        string_buf.push_str(">");
                    },
                    &Token::BeginTag(ref name) => try_w_s!(string_buf, "<{}", name.as_slice()),
                    &Token::EndTag(_self_close) => string_buf.push_str(">"),
                    &Token::CloseTag(ref name) => try_w_s!(string_buf, "</{}>", name.as_slice()),
                    &Token::BeginAttribute(ref name, ref content) => match content {
                        &Content::String(ref content) => try_w_s!(string_buf, " {}=\\\"{}", name.as_slice(), content),
                        &Content::Placeholder(ref placeholder) => {
                            match find_param(placeholder, params, &scopes) {
                                Some((param_ty, Some(&ContentType::String(false)))) | Some((param_ty, None)) => {
                                    try_w_s!(string_buf, " {}=\\\"{{}}", name.as_slice());
                                    if let ParamTy::Param = param_ty {
                                        fmt_args.push(format!("self.{}", placeholder));
                                    } else {
                                        fmt_args.push(placeholder.into());
                                    }
                                },
                                Some((param_ty, Some(&ContentType::String(true)))) => {
                                    try_w_s!(string_buf, " {}=\\\"", name.as_slice());
                                    try!(try_write_and_clear_fmt(&mut func, &mut string_buf, &mut fmt_args));

                                    if let ParamTy::Param = param_ty {
                                        try_w!(func, "if let Some(ref val) = self.{} {{", placeholder);
                                    } else {
                                        try_w!(func, "if let Some(val) = {} {{", placeholder);
                                    }

                                    try_w!(func.indented_line(), "try!(write!(writer, \"{{}}\", val));");
                                    try_w!(func, "}}");
                                },
                                Some((param_ty, Some(&ContentType::Template(false)))) => {
                                    try_w_s!(string_buf, " {}=\\\"", name.as_slice());
                                    try!(try_write_and_clear_fmt(&mut func, &mut string_buf, &mut fmt_args));

                                    if let ParamTy::Param = param_ty {
                                        try_w!(func, "try!(self.{}.render_to(writer));", placeholder);
                                    } else {
                                        try_w!(func, "try!({}.render_to(writer));", placeholder);
                                    }
                                },
                                Some((param_ty, Some(&ContentType::Template(true)))) => {
                                    try_w_s!(string_buf, " {}=\\\"", name.as_slice());
                                    try!(try_write_and_clear_fmt(&mut func, &mut string_buf, &mut fmt_args));

                                    if let ParamTy::Param = param_ty {
                                        try_w!(func, "if let Some(template) = self.{} {{", placeholder);
                                    } else {
                                        try_w!(func, "if let Some(template) = {} {{", placeholder);
                                    }

                                    try_w!(func.indented_line(), "try!(template.render_to(writer));");
                                    try_w!(func, "}}");
                                },
                                Some((_, Some(_ty))) => return Err(Error::CannotBeRendered(placeholder.into())),
                                None => return Err(Error::UndefinedPlaceholder(placeholder.into()))
                            }
                        }
                    },
                    &Token::AppendToAttribute(ref text) | &Token::Text(ref text) => match text {
                        &Content::String(ref content) => try_w_s!(string_buf, "{}", content),
                        &Content::Placeholder(ref placeholder) => {
                            match find_param(placeholder, params, &scopes) {
                                Some((param_ty, Some(&ContentType::String(false)))) | Some((param_ty, None)) => {
                                    string_buf.push_str("{}");
                                    if let ParamTy::Param = param_ty {
                                        fmt_args.push(format!("self.{}", placeholder));
                                    } else {
                                        fmt_args.push(placeholder.into());
                                    }
                                },
                                Some((param_ty, Some(&ContentType::String(true)))) => {
                                    try!(try_write_and_clear_fmt(&mut func, &mut string_buf, &mut fmt_args));

                                    if let ParamTy::Param = param_ty {
                                        try_w!(func, "if let Some(ref val) = self.{} {{", placeholder);
                                    } else {
                                        try_w!(func, "if let Some(val) = {} {{", placeholder);
                                    }

                                    try_w!(func.indented_line(), "try!(write!(writer, \"{{}}\", val));");
                                    try_w!(func, "}}");
                                },
                                Some((param_ty, Some(&ContentType::Template(false)))) => {
                                    try!(try_write_and_clear_fmt(&mut func, &mut string_buf, &mut fmt_args));

                                    if let ParamTy::Param = param_ty {
                                        try_w!(func, "try!(self.{}.render_to(writer));", placeholder);
                                    } else {
                                        try_w!(func, "try!({}.render_to(writer));", placeholder);
                                    }
                                },
                                Some((param_ty, Some(&ContentType::Template(true)))) => {
                                    try!(try_write_and_clear_fmt(&mut func, &mut string_buf, &mut fmt_args));

                                    if let ParamTy::Param = param_ty {
                                        try_w!(func, "if let Some(template) = self.{} {{", placeholder);
                                    } else {
                                        try_w!(func, "if let Some(template) = {} {{", placeholder);
                                    }

                                    try_w!(func.indented_line(), "try!(template.render_to(writer));");
                                    try_w!(func, "}}");
                                },
                                Some((_, Some(_ty))) => return Err(Error::CannotBeRendered(placeholder.into())),
                                None => return Err(Error::UndefinedPlaceholder(placeholder.into()))
                            }
                        }
                    },
                    &Token::EndAttribute => string_buf.push_str("\\\""),
                    &Token::Scope(Scope::If(ref cond)) => {
                        scopes.push(None);
                        try!(try_write_and_clear_fmt(&mut func, &mut string_buf, &mut fmt_args));

                        {
                            let mut line = func.begin_line();
                            try_w!(line, "if ");
                            try!(self.eval_logic(&mut line, true, &cond.flattened(), params));
                            try_w!(line, " {{");
                        }

                        func.indent();
                    },
                    &Token::Scope(Scope::ForEach(ref collection, ref element, ref opt_key)) => {
                        try!(try_write_and_clear_fmt(&mut func, &mut string_buf, &mut fmt_args));

                        match find_param(collection, params, &scopes) {
                            Some((param_ty, content_ty)) => {
                                match content_ty {
                                    Some(&ContentType::Collection(ref ty, optional)) => {
                                        let mut line = func.begin_line();

                                        if let &Some(ref key) = opt_key {
                                            try_w!(line, "for ({}, {})", key, element);
                                        } else {
                                            try_w!(line, "for {}", element);
                                        }

                                        if let ParamTy::Param = param_ty {
                                            try_w!(line, " in self.{}", collection);
                                        } else {
                                            try_w!(line, " in {}", collection);
                                        }

                                        if optional {
                                            try_w!(line, ".iter().flat_map(|c| c");
                                        }

                                        if opt_key.is_some() {
                                            try_w!(line, ".key_values()");
                                        } else {
                                            try_w!(line, ".values()");
                                        }

                                        if optional {
                                            try_w!(line, ") {{\n");
                                        } else {
                                            try_w!(line, " {{\n");
                                        }

                                        if let &Some(ref ty) = ty {
                                            scopes.push(Some((element, ty, opt_key.clone())));
                                        } else {
                                            return Err(Error::UnknownType(collection.into()))
                                        }
                                    },
                                    Some(_ty) => return Err(Error::UnexpectedType(collection.into(), ContentType::Collection(None, false))),
                                    None => return Err(Error::UnexpectedType(collection.into(), ContentType::Collection(None, false)))
                                }
                            },
                            None => return Err(Error::UndefinedPlaceholder(collection.into())),
                        }

                        func.indent();
                    },
                    &Token::End => {
                        try!(try_write_and_clear_fmt(&mut func, &mut string_buf, &mut fmt_args));

                        func.unindent();
                        try_w!(func, "}}");
                    }
                }
            }

            try!(write_and_clear_fmt(&mut func, &mut string_buf, &mut fmt_args));
        }

        try_w!(w, "}}");
        w.unindent();

        write!(w, "}}").map_err(|e| e.into())
    }

    fn build_module<W, F>(&self, w: &mut Writer<W>, build_templates: F) -> Result<(), Error> where
        W: Write,
        F: FnOnce(&mut Writer<W>) -> Result<(), Error>
    {
        if let Some((ref module, ref visibility)) = self.named_module {
            match visibility {
                &Visibility::Public => try_w!(w, "pub mod {} {{", module),
                &Visibility::Private => try_w!(w, "mod {} {{", module)
            }
            w.indent();
        }

        try!(build_templates(w));

        if self.named_module.is_some() {
            try_w!(w, "}}");
        }

        Ok(())
    }
}

fn try_write_and_clear_fmt<W: Write>(w: &mut Writer<W>, buf: &mut String, args: &mut Vec<String>) -> Result<(), Error> {
    if buf.len() > 0 {
        if args.len() == 0 {
            try_w!(w, "try!(write!(writer, \"{}\"));", buf);
        } else {
            try_w!(w, "try!(write!(writer, \"{}\", {}));", buf, args.connect(", "));
        }
        buf.clear();
        args.clear();
    }

    Ok(())
}

fn write_and_clear_fmt<W: Write>(w: &mut Writer<W>, buf: &mut String, args: &mut Vec<String>) -> Result<(), Error> {
    if buf.len() > 0 {
        if args.len() == 0 {
            try_w!(w, "write!(writer, \"{}\")", buf);
        } else {
            try_w!(w, "write!(writer, \"{}\", {})", buf, args.connect(", "));
        }
        buf.clear();
        args.clear();
    }

    Ok(())
}

fn write_ty<W: Write>(w: &mut Line<W>, ty: &ContentType) -> Result<(), Error> {
    if ty.is_optional() {
        try_w!(w, "Option<");
    }

    match ty {
        &ContentType::String(_) => try_w!(w, "::symbiosis_rust::Content<'a>"),
        &ContentType::Bool => try_w!(w, "bool"),
        &ContentType::Template(_) => try_w!(w, "&'a ::symbiosis_rust::Template"),
        &ContentType::Collection(Some(ref inner), _) => {
            try_w!(w, "&'a ::symbiosis_rust::Collection<'a, ");
            try!(write_ty(w, inner));
            try_w!(w, ">");
        },
        &ContentType::Collection(None, _) => {
            return Err(Error::UnknownType("".into()))
        }
    }

    if ty.is_optional() {
        try_w!(w, ">");
    }

    Ok(())
}

fn find_param<'a>(param: &StrTendril, params: &'a HashMap<StrTendril, ContentType>, scopes: &[Option<(&str, &'a ContentType, Option<StrTendril>)>]) -> Option<(ParamTy, Option<&'a ContentType>)> {
    for scope in scopes.iter().rev() {
        if let &Some((ref name, ref ty, ref opt_key)) = scope {
            if *name == &**param {
                return Some((ParamTy::Scope,Some(ty)));
            } else if let &Some(ref key) = opt_key {
                if key == param {
                    return Some((ParamTy::Scope, None))
                }
            }
        }
    }

    params.get(param).map(|t| (ParamTy::Param,Some(t)))
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
                '\n' => try!(f.write_str("\\n")),
                '\t' => try!(f.write_str("\\t")),
                '{' => try!(f.write_str("&#123;")),
                '}' => try!(f.write_str("&#125;")),
                c => try!(f.write_char(c))
            }
        }

        Ok(())
    }
}
