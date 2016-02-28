use std::io::Write;
use std::collections::HashMap;

use codegen::{Writer, Line, Name, Type, Structs};

use rust::Error;

pub fn write_structs<W: Write>(writer: &mut Writer<W>, structs: Structs, public: bool) -> Result<(), Error> {
    for s in structs {
        if public {
            if requires_lifetime(&s.fields) {
                try_w!(writer, "pub struct {}<'a> {{", s.name);
            } else {
                try_w!(writer, "pub struct {};", s.name);
            }
        } else {
            if requires_lifetime(&s.fields) {
                try_w!(writer, "struct {}<'a> {{", s.name);
            } else {
                try_w!(writer, "struct {};", s.name);
            }
        }

        {
            let mut block = writer.block();
            for (parameter, ty) in &s.fields {
                let mut line = block.begin_line();
                try_w!(line, "pub {}: ", parameter);
                match write_ty(&mut line, ty) {
                    Err(Error::UnknownType(_)) => return Err(Error::UnknownType((**parameter).into())),
                    r => try!(r)
                }
                try_w!(line, ",");
            }
        }
        
        if s.fields.len() > 0 {
            try_w!(writer, "}}\n");
        }
    }

    Ok(())
}

pub fn requires_lifetime(params: &HashMap<Name, Type>) -> bool {
    params.len() > 0
}

fn write_ty<W: Write>(w: &mut Line<W>, ty: &Type) -> Result<(), Error> {
    if ty.is_optional() {
        try_w!(w, "Option<");
    }

    match ty {
        &Type::Content(_) => try_w!(w, "::symbiosis_static::Content<'a>"),
        &Type::Bool => try_w!(w, "bool"),
        &Type::Collection(Some(ref inner), _) => {
            try_w!(w, "::symbiosis_static::Collection<'a, ");
            try!(write_ty(w, inner));
            try_w!(w, ">");
        },
        &Type::Struct(ref name, _) => {
            try_w!(w, "{}<'a>", name);
        },
        &Type::Collection(None, _) => {
            return Err(Error::UnknownType("".into()))
        },
    }

    if ty.is_optional() {
        try_w!(w, ">");
    }

    Ok(())
}
