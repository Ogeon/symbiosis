use std::io::{self, Write};

use codegen::{Params, Writer, Line, ContentType};

use rust::Error;

struct Struct<'a> {
    entries: Vec<(&'a str, Type<'a>)>,
    requires_lifetime: bool,
}

enum Type<'a> {
    Content(bool),
    Bool,
    Struct(&'a str, bool, bool),
    Collection(Box<Type<'a>>, bool),
}

impl<'a> Type<'a> {
    fn requires_lifetime_param(&self) -> bool {
        match *self {
            Type::Content(_) => true,
            Type::Bool => false,
            Type::Struct(_, lifetime, _) => lifetime,
            Type::Collection(_, _) => true,
        }
    }

    fn is_optional(&self) -> bool {
        match *self {
            Type::Content(optional) => optional,
            Type::Bool => false,
            Type::Struct(_, _, optional) => optional,
            Type::Collection(_, optional) => optional,
        }
    }
}

pub fn write_structs<W: Write>(writer: &mut Writer<W>, template_name: &str, params: &Params, public: bool) -> Result<bool, Error> {
    let structs = params.flatten(template_name.into());

    let mut requires_lifetime = false;

    for (name, entries) in structs {
        if name == template_name && entries.len() > 0 {
            requires_lifetime = true;
        }

        if public {
            if entries.len() > 0 {
                try_w!(writer, "pub struct {}<'a> {{", name);
            } else {
                try_w!(writer, "pub struct {};", name);
            }
        } else {
            if entries.len() > 0 {
                try_w!(writer, "struct {}<'a> {{", name);
            } else {
                try_w!(writer, "struct {};", name);
            }
        }

        {
            let mut block = writer.block();
            for (parameter, ty) in &entries {
                let mut line = block.begin_line();
                try_w!(line, "pub {}: ", parameter);
                match write_ty(&mut line, ty) {
                    Err(Error::UnknownType(_)) => return Err(Error::UnknownType(parameter.into())),
                    r => try!(r)
                }
                try_w!(line, ",");
            }
        }
        
        if entries.len() > 0 {
            try_w!(writer, "}}\n");
        }
    }

    Ok(requires_lifetime)
}

fn write_ty<W: Write>(w: &mut Line<W>, ty: &ContentType) -> Result<(), Error> {
    if ty.is_optional() {
        try_w!(w, "Option<");
    }

    match ty {
        &ContentType::String(_) => try_w!(w, "::symbiosis_rust::Content<'a>"),
        &ContentType::Bool => try_w!(w, "bool"),
        &ContentType::Collection(Some(ref inner), _) => {
            try_w!(w, "::symbiosis_rust::Collection<'a, ");
            try!(write_ty(w, inner));
            try_w!(w, ">");
        },
        &ContentType::Struct(Some(ref name), _, _) => {
            try_w!(w, "{}<'a>", name);
        },
        &ContentType::Collection(None, _) | &ContentType::Struct(None, _, _) => {
            return Err(Error::UnknownType("".into()))
        },
    }

    if ty.is_optional() {
        try_w!(w, ">");
    }

    Ok(())
}
