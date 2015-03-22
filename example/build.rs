extern crate symbiosis;

use std::path::Path;
use std::fs::{File, create_dir_all};
use std::default::Default;

use symbiosis::{TemplateGroup, JavaScript, Rust};

fn main() {
    let out_dir = std::env::var("OUT_DIR").unwrap();
    let rust_dest = Path::new(&out_dir).join("symbiosis/");
    create_dir_all(&rust_dest);
    let js_dest = Path::new("templates/js");
    create_dir_all(&js_dest);

    let mut templates = TemplateGroup::new();

    if let Err(e) = templates.parse_directory("templates/html/") {
        panic!("failed to precompile templates/html/: {}", e);
    }

    let js = JavaScript {
        namespace: Some("templates"),
        ..Default::default()
    };

    let rust = Rust { ..Default::default() };

    if let Err(e) = File::create(rust_dest.join("templates.rs")).and_then(|mut file| templates.emit_code(&mut file, &rust)) {
        panic!("failed to create symbiosis/templates.rs: {}", e)
    }

    if let Err(e) = File::create(js_dest.join("templates.js")).and_then(|mut file| templates.emit_code(&mut file, &js)) {
        panic!("failed to create templates/js/templates.js: {}", e)
    }
}