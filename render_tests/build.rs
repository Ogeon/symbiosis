extern crate symbiosis;
extern crate serde_json;

use std::path::Path;
use std::fs::{File, create_dir_all, read_dir};
use std::io::{self, Write};
use std::default::Default;
use std::collections::BTreeMap;
use std::fmt::Write as FmtWrite;

use symbiosis::TemplateGroup;
use symbiosis::codegen::{Struct, Type};
use symbiosis::rust::{Rust, Visibility};
use symbiosis::javascript::{JavaScript};

use serde_json::Value;

fn main() {
    let out_dir = std::env::var("OUT_DIR").unwrap();
    let rust_dest = Path::new(&out_dir).join("symbiosis/");
    if let Err(e) = create_dir_all(&rust_dest) {
        panic!("failed to create '{}': {}", rust_dest.display(), e);
    }
    let rust_path = rust_dest.join("templates.rs");
    let mut rust_file = match File::create(&rust_path) {
        Ok(f) => f,
        Err(e) => panic!("failed to create '{}': {}", rust_path.display(), e),
    };

    let mut rust_test_fn = String::new();

    let js = JavaScript {
        namespace: None,
        ..Default::default()
    };

    for entry in read_dir("tests").expect("couldn't start reading from dir 'tests'") {
        let entry = entry.expect("couldn't read dir 'tests'");
        if entry.file_type().expect("couldn't get file type").is_dir() {
            let mut templates = TemplateGroup::new();
            let path = entry.path();
            let test_name = path.file_name().expect("couldn't read dir name").to_string_lossy();
            let rust = Rust { named_module: Some((&test_name, Visibility::Public)), ..Default::default() };

            let values_path = path.join("values.json");
            let values: BTreeMap<String, Value> = match File::open(&values_path).map_err(From::from).and_then(|file| serde_json::from_reader(file)) {
                Ok(v) => v,
                Err(e) => panic!("couldn't open and decode {}: {}", values_path.display(), e),
            };

            let template_path = path.join("template.html");
            if let Err(e) = templates.parse_file("Template".into(), &template_path) {
                panic!("couldn't read {}: {}", template_path.display(), e);
            }
            templates.emit_code(&mut rust_file, &rust).expect("failed to emit rust code");

            let js_path = path.join("main.js");
            let mut js_file = match File::create(&js_path) {
                Ok(f) => f,
                Err(e) => panic!("failed to create '{}': {}", js_path.display(), e),
            };
            templates.emit_code(&mut js_file, &js).expect("failed to emit js code");
            writeln!(js_file, "var template = new Template();").expect("failed to write js test function");

            let template = templates.get_struct("Template").expect("struct 'Tempalte' is missing");
            let _ = writeln!(rust_test_fn, r#"    ::test("{0:}", || {0:}::Template{{"#, test_name);
            print_assigns(&mut rust_test_fn, &mut js_file, &values, template, &templates, &test_name).expect("failed to write js test function");
            let _ = writeln!(rust_test_fn, r#"    }});"#);

            writeln!(js_file, r#"var div = document.createElement("div");"#).expect("failed to write js test function");
            writeln!(js_file, "template.render_to(div);").expect("failed to write js test function");
            writeln!(js_file, r#"require("fs").write("tests/{}/javascript.html", div.innerHTML, "w");"#, test_name).expect("failed to write js test function");
            writeln!(js_file, "phantom.exit();").expect("failed to write js test function");

        }
    }

    writeln!(rust_file, "pub fn run_tests() {{\n{}}}", rust_test_fn).expect("couldn't write rust test function");
}

fn print_assigns(rust_test_fn: &mut String, js_file: &mut File, values: &BTreeMap<String, Value>, current_struct: &Struct, templates: &TemplateGroup, module: &str) -> io::Result<()> {
    for (key, value) in values {
        let ty = if let Some(ty) = current_struct.fields.get(&**key) {
            ty
        } else {
            panic!("expected template to have '{}'", key);
        };

        try!(write!(js_file, "template.{} = ", key));
        let _ = write!(rust_test_fn, "        {}: ", key);

        try!(print_literal(rust_test_fn, js_file, value, ty, templates, module));
        try!(writeln!(js_file, ";"));
    }
    Ok(())
}

fn print_fields(rust_test_fn: &mut String, js_file: &mut File, values: &BTreeMap<String, Value>, current_struct: &Struct, templates: &TemplateGroup, module: &str) -> io::Result<()> {
    for (key, value) in values {
        let ty = if let Some(ty) = current_struct.fields.get(&**key) {
            ty
        } else {
            panic!("expected template to have '{}'", key);
        };

        try!(write!(js_file, r#""{}": "#, key));
        let _ = write!(rust_test_fn, "        {}: ", key);

        try!(print_literal(rust_test_fn, js_file, value, ty, templates, module));
        try!(write!(js_file, ","));
    }
    Ok(())
}

fn print_literal(rust_test_fn: &mut String, js_file: &mut File, value: &Value, ty: &Type, templates: &TemplateGroup, module: &str) -> io::Result<()> {
    let _ = match (value, ty) {
        (&Value::String(ref s), _) => {
            try!(write!(js_file, r#""{}""#, s));
            if ty.is_optional() {
                writeln!(rust_test_fn, r#"Some("{}".into()),"#, s)
            } else {
                writeln!(rust_test_fn, r#""{}".into(),"#, s)
            }
        },
        (&Value::U64(n), _) => {
            try!(write!(js_file, "{}", n));
            if ty.is_optional() {
                writeln!(rust_test_fn, "Some({}.into()),", n)
            } else {
                writeln!(rust_test_fn, "{}.into(),", n)
            }
        },
        (&Value::I64(n), _) => {
            try!(write!(js_file, "{}", n));
            if ty.is_optional() {
                writeln!(rust_test_fn, "Some({}.into()),", n)
            } else {
                writeln!(rust_test_fn, "{}.into(),", n)
            }
        },
        (&Value::F64(n), _) => {
            try!(write!(js_file, "{}", n));
            if ty.is_optional() {
                writeln!(rust_test_fn, "Some({}.into()),", n)
            } else {
                writeln!(rust_test_fn, "{}.into(),", n)
            }
        },
        (&Value::Null, _) => {
            try!(write!(js_file, "null"));
            writeln!(rust_test_fn, "None,")
        },
        (&Value::Bool(b), _) => {
            try!(write!(js_file, "{}", b));
            writeln!(rust_test_fn, "{},", b)
        },
        (&Value::Object(ref object), &Type::Struct(ref name, optional)) => {
            try!(write!(js_file, "{{"));
            if optional {
                let _ = write!(rust_test_fn, "Some(");
            }
            let _ = writeln!(rust_test_fn, "{}::{} {{", module, name);
            try!(print_fields(rust_test_fn, js_file, object, templates.get_struct(name).expect("missing struct"), templates, module));
            try!(write!(js_file, "}}"));
            let _ = write!(rust_test_fn, "}}");
            if optional {
                writeln!(rust_test_fn, "),")
            } else {
                writeln!(rust_test_fn, ",")
            }
        },
        (&Value::Array(ref array), &Type::Collection(Some(ref inner), optional)) => {
            try!(write!(js_file, "["));
            if optional {
                let _ = write!(rust_test_fn, "Some(");
            }
            let _ = writeln!(rust_test_fn, "vec![");
            for value in array {
                try!(print_literal(rust_test_fn, js_file, value, &**inner, templates, module));
                try!(write!(js_file, ","));
            }
            try!(write!(js_file, "]"));
            let _ = write!(rust_test_fn, "].into()");
            if optional {
                writeln!(rust_test_fn, "),")
            } else {
                writeln!(rust_test_fn, ",")
            }
        },
        (&Value::Object(ref object), &Type::Collection(Some(ref inner), optional)) => {
            try!(write!(js_file, "{{"));
            if optional {
                let _ = write!(rust_test_fn, "Some(");
            }
            let _ = writeln!(rust_test_fn, "{{ let mut map = ::std::collections::BTreeMap::new();");
            for (key, value) in object {
                try!(write!(js_file, r#""{}": "#, key));
                let _ = write!(rust_test_fn, r#"map.insert("{}".into(), "#, key);
                try!(print_literal(rust_test_fn, js_file, value, &**inner, templates, module));
                try!(write!(js_file, ","));
                let _ = writeln!(rust_test_fn, ");");
            }
            try!(write!(js_file, "}}"));
            let _ = write!(rust_test_fn, "map.into() }}");
            if optional {
                writeln!(rust_test_fn, "),")
            } else {
                writeln!(rust_test_fn, ",")
            }
        },
        (&Value::Array(_), ty) => panic!("an array can't be used as {}", ty),
        (&Value::Object(_), ty) => panic!("an object can't be used as {}", ty),
    };
    Ok(())
}
