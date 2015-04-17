extern crate symbiosis;

use std::path::Path;
use std::fs::{File, create_dir_all};
use std::io::Read;
use std::default::Default;

use symbiosis::TemplateGroup;
use symbiosis::rust::{self, Rust};
use symbiosis::javascript::{self, JavaScript};

fn main() {
    let out_dir = std::env::var("OUT_DIR").unwrap();
    let rust_dest = Path::new(&out_dir).join("symbiosis/");
    if let Err(e) = create_dir_all(&rust_dest) {
        panic!("failed to create Symbiosis output directory: {}", e);
    }
    let js_dest = Path::new("res");

    let mut templates = TemplateGroup::new();

    if let Err(e) = templates.parse_directory("templates/shared") {
        panic!("failed to precompile templates/shared: {}", e);
    }

    let js = JavaScript {
        namespace: Some("templates"),
        ..Default::default()
    };

    let rust = Rust { ..Default::default() };

    if let Err(e) = File::create(js_dest.join("templates.js")).map_err(|e| javascript::Error::Io(e)).and_then(|mut file| templates.emit_code(&mut file, &js)) {
        panic!("failed to create res/templates.js: {}", e);
    }

    let mut source = String::new();
    if let Err(e) = File::open("templates/Document.html").and_then(|mut f| f.read_to_string(&mut source)) {
        panic!("failed to read templates/Document.html: {}", e);
    }

    if let Err(e) = templates.parse_string("Document".into(), source) {
        panic!("failed to parse templates/Document.html: {}", e);
    }

    if let Err(e) = File::create(rust_dest.join("templates.rs")).map_err(|e| rust::Error::Io(e)).and_then(|mut file| templates.emit_code(&mut file, &rust)) {
        panic!("failed to create symbiosis/templates.rs: {}", e);
    }
}