extern crate symbiosis;

use std::path::Path;
use std::fs::{File, create_dir_all};

fn main() {
    let out_dir = std::env::var("OUT_DIR").unwrap();
    let rust_dest = Path::new(&out_dir).join("symbiosis/");
    create_dir_all(&rust_dest);
    let js_dest = Path::new("templates/js");
    create_dir_all(&js_dest);

    let mut templates = match symbiosis::parse_directory("templates/html/") {
        Ok(gen) => gen,
        Err(e) => panic!("failed to precompile templates/html/: {}", e)
    };

    templates.js_namespace("templates", false);

    if let Err(e) = File::create(rust_dest.join("templates.rs")).and_then(|mut file| templates.emit_rust(&mut file)) {
        panic!("failed to create symbiosis/templates.rs: {}", e)
    }

    if let Err(e) = File::create(js_dest.join("templates.js")).and_then(|mut file| templates.emit_js(&mut file)) {
        panic!("failed to create templates/js/templates.js: {}", e)
    }
}