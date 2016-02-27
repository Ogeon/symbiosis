extern crate lalrpop;
extern crate symbiosis_pattern;
extern crate aster;
extern crate syntex_syntax as syntax;

use std::fs::File;
use std::path::Path;
use std::io::Write;

use symbiosis_pattern::{AnnotatedPattern, build_ast};

use syntax::ast::{ItemKind, Mod, Item};
use syntax::ptr::P;
use syntax::codemap::DUMMY_SP;

fn main() {
    lalrpop::process_root().unwrap();

    let out_dir = ::std::env::var("OUT_DIR").unwrap();
    let dest_path = Path::new(&out_dir).join("pattern.rs");
    let mut file = File::create(dest_path).expect("couldn't create pattern.rs");

    build_pattern(&mut file, "not", r#"NotArgs { cond: input }"#);
    build_pattern(&mut file, "foreach", r#"
        ForEachArgs {
            key: (Key { k: input {"=>"} })?
            element: input
            {"in"}
            collection: input
        }
    "#);
    build_pattern(&mut file, "struct_name", r#"
        StructNameArgs {
            path: input
            {"is"}
            name: string
        }
    "#);
}

fn build_module(name: &str, items: Vec<P<Item>>) -> P<Item> {
    aster::AstBuilder::new().item().pub_().build_item_kind(name, ItemKind::Mod(Mod {
        inner: DUMMY_SP,
        items: items,
    }))
}

fn build_pattern(file: &mut File, name: &str, pattern: &str) {
    let module = match AnnotatedPattern::from_str(pattern) {
        Ok(p) => build_module(name, build_ast(&p)),
        Err(e) => panic!("failed to parse '{}' pattern: {:?}", name, e),
    };
    if let Err(e) = writeln!(file, "{}", syntax::print::pprust::item_to_string(&module)) {
        panic!("could not write '{}' module: {:?}", name, e);
    }
}