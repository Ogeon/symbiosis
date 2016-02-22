use std::io::{self, Write};
use std::fmt::Write as FmtWrite;
use std::default::Default;
use std::fmt;

use tendril::StrTendril;

use codegen::{Codegen, Writer, Line, Logic, Token, Scope, ContentType, Content, Path, Params};

mod types;

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

///Code generator for Rust templates.
pub struct Rust<'a> {
    ///Generate a submodule. Defaults to none and should only be used when a
    ///whole module is generated.
    pub named_module: Option<(&'a str, Visibility)>,

    ///The template visibility. Defaults to public.
    pub visibility: Visibility
}

impl<'a> Rust<'a> {
    fn eval_logic<W: Write>(&self, w: &mut Line<W>, first: bool, cond: &Logic, params: &Params) -> Result<(), Error> {
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
                    Some(&ContentType::String(false)) | Some(&ContentType::Collection(_, false)) | Some(&ContentType::Struct(_, _, false)) => try_w!(w, "true"),
                    Some(&ContentType::String(true)) | Some(&ContentType::Collection(_, true)) | Some(&ContentType::Struct(_, _, true)) => try_w!(w, "self.{}.is_some()", val),
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
    
    fn build_template<W: Write>(&self, w: &mut Writer<W>, name: &str, params: &Params, tokens: &[Token]) -> Result<(), Error> {
        let public = match (&self.visibility, &self.named_module) {
            (&Visibility::Public, _) => true,
            (_, &Some(_)) => true,
            _ => false
        };

        let mut string_buf = String::new();
        let mut fmt_args = vec![];
        let mut scopes = vec![];

        let has_lifetime = try!(types::write_structs(w, name, params, public));

        if has_lifetime {
            try_w!(w, "impl<'a> ::std::fmt::Display for {}<'a> {{", name);
        } else {
            try_w!(w, "impl ::std::fmt::Display for {} {{", name);
        }

        w.indent();

        try_w!(w, "fn fmt(&self, writer: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {{");

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
                    &Token::Comment(ref comment) => try_w_s!(string_buf, "<!--{}-->", comment),
                    &Token::BeginTag(ref name) => try_w_s!(string_buf, "<{}", name),
                    &Token::EndTag(_self_close) => string_buf.push_str(">"),
                    &Token::CloseTag(ref name) => try_w_s!(string_buf, "</{}>", name),
                    &Token::BeginAttribute(ref name, ref content) => match content {
                        &Content::String(ref content) => try_w_s!(string_buf, " {}=\\\"{}", name, content.to_string()),
                        &Content::Placeholder(ref placeholder, _) => {
                            match find_param(placeholder, params, &scopes) {
                                Some((access_path, Some((&ContentType::String(_), false)))) | Some((access_path, None)) => {
                                    try_w_s!(string_buf, " {}=\\\"{{}}", name);
                                    fmt_args.push(access_path);
                                },
                                Some((access_path, Some((&ContentType::String(_), true)))) => {
                                    try_w_s!(string_buf, " {}=\\\"", name);
                                    try!(try_write_and_clear_fmt(&mut func, &mut string_buf, &mut fmt_args));

                                    try_w!(func, "if let Some(ref val) = {} {{", access_path);

                                    try_w!(func.indented_line(), "try!(write!(writer, \"{{}}\", val));");
                                    try_w!(func, "}}");
                                },
                                Some((_, Some(ty))) => {
                                    println!("non-text type: {:?}", ty);
                                    return Err(Error::CannotBeRendered(placeholder.to_string()))
                                },
                                None => return Err(Error::UndefinedPlaceholder(placeholder.to_string()))
                            }
                        }
                    },
                    &Token::AppendToAttribute(ref text) | &Token::Text(ref text) => match text {
                        &Content::String(ref content) => try_w_s!(string_buf, "{}", content.to_string()),
                        &Content::Placeholder(ref placeholder, _) => {
                            match find_param(placeholder, params, &scopes) {
                                Some((access_path, Some((&ContentType::String(_), false)))) | Some((access_path, None)) => {
                                    string_buf.push_str("{}");
                                    fmt_args.push(access_path);
                                },
                                Some((access_path, Some((&ContentType::String(_), true)))) => {
                                    try!(try_write_and_clear_fmt(&mut func, &mut string_buf, &mut fmt_args));
                                    try_w!(func, "if let Some(ref val) = {} {{", access_path);
                                    try_w!(func.indented_line(), "try!(write!(writer, \"{{}}\", val));");
                                    try_w!(func, "}}");
                                },
                                Some((_, Some(ty))) => {
                                    println!("non-text type: {:?}", ty);
                                    return Err(Error::CannotBeRendered(placeholder.to_string()))
                                },
                                None => return Err(Error::UndefinedPlaceholder(placeholder.to_string()))
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
                            Some((access_path, content_ty)) => {
                                match content_ty {
                                    Some((&ContentType::Collection(ref ty, _), optional)) => {
                                        let mut line = func.begin_line();

                                        if let &Some(ref key) = opt_key {
                                            try_w!(line, "for ({}, {})", key, element);
                                        } else {
                                            try_w!(line, "for {}", element);
                                        }

                                        try_w!(line, " in {}", access_path);

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
                                            scopes.push(Some(ForEachLevel {
                                                alias: element.clone(),
                                                alias_type: ty,
                                                key: opt_key.clone(),
                                            }));
                                        } else {
                                            return Err(Error::UnknownType(collection.to_string()))
                                        }
                                    },
                                    Some(_ty) => return Err(Error::UnexpectedType(collection.to_string(), ContentType::Collection(None, false))),
                                    None => return Err(Error::UnexpectedType(collection.to_string(), ContentType::Collection(None, false)))
                                }
                            },
                            None => return Err(Error::UndefinedPlaceholder(collection.to_string())),
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
            try_w!(w, "try!(write!(writer, \"{}\", {}));", buf, args.join(", "));
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
            try_w!(w, "write!(writer, \"{}\", {})", buf, args.join(", "));
        }
        buf.clear();
        args.clear();
    }

    Ok(())
}

fn find_param<'a>(path: &Path, params: &'a Params, scopes: &[Option<ForEachLevel<'a>>]) -> Option<(String, Option<(&'a ContentType, bool)>)> {
    let param = path.first().expect("empty path");

    for scope in scopes.iter().rev() {
        if let Some(ref for_each) = *scope {
            if *for_each.alias == **param {
                let access_path = vec![(param, for_each.alias_type.is_optional())];
                return make_access_path(path, access_path, for_each.alias_type);
            } else if let Some(ref key) = for_each.key {
                if key == param {
                    if path.len() == 1 {
                        return Some((key.into(), None));
                    } else {
                        return None;
                    }
                }
            }
        }
    }

    if let Some(ty) = params.get(&param.clone().into()) {
        make_access_path(path, vec![(&"self".into(), false), (param, ty.is_optional())], ty)
    } else {
        None
    }
}

fn make_access_path<'a>(path: &Path, base: Vec<(&StrTendril, bool)>, base_type: &'a ContentType) -> Option<(String, Option<(&'a ContentType, bool)>)> {
    let mut access_path = base;
    let mut current_type = Some(base_type);

    if path.len() > 1 {
        let mut current_params = match *base_type {
            ContentType::Struct(_, ref params, _) => Some(params),
            _ => return None,
        };
        let mut path = path[1..].iter();

        while let (Some(params), Some(part)) = (current_params.take(), path.next()) {
            let next_type = if let Some(ty) = params.get(&part.clone().into()) {
                ty
            } else {
                return None;
            };

            access_path.push((part, next_type.is_optional()));
            current_params = match *next_type {
                ContentType::Struct(_, ref params, _) => Some(params),
                _ => None,
            };
            current_type = Some(next_type);
        }
    }

    if let Some(ty) = current_type {
        let mut result_type = ty.shallow_clone();
        let mut access_path = access_path.into_iter().rev();
        let mut path = access_path.next().map(|(name, _)| name.into()).unwrap_or(String::new());

        for (name, optional) in access_path {
            if optional {
                if result_type.is_optional() {
                    path = format!("{}.as_ref().and_then(|v| &v.{})", name, path);
                } else {
                    path = format!("{}.as_ref().map(|v| &v.{})", name, path);
                }
                result_type.set_optional(true);
            } else {
                path = format!("{}.{}", name, path);
            }
        }

        Some((path, Some((ty, result_type.is_optional()))))
    } else {
        None
    }
}

struct Sanitized<S>(S);

impl<S> fmt::Display for Sanitized<S> where S: AsRef<str> {
    fn fmt(&self, f: &mut fmt::Formatter) ->  fmt::Result {
        for c in self.0.as_ref().chars() {
            match c {
                '"' => try!(f.write_str("\\\"")),
                '\n' => try!(f.write_str("\\n")),
                '\t' => try!(f.write_str("\\t")),
                '\\' => try!(f.write_str("\\\\")),
                '{' => try!(f.write_str("{{")),
                '}' => try!(f.write_str("}}")),
                c => try!(f.write_char(c))
            }
        }

        Ok(())
    }
}

struct ForEachLevel<'a> {
    alias: StrTendril,
    alias_type: &'a ContentType,
    key: Option<StrTendril>,
}
