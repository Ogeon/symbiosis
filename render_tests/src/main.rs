extern crate serde_json;
extern crate symbiosis_static;
extern crate symbiosis_dynamic;

use symbiosis_dynamic::Parser;

use std::fs;
use std::io::{Read, Write};
use std::fmt::Display;
use std::path::PathBuf;

mod templates;

fn main() {
    templates::run_tests();
}

fn test<T: Display, F: FnOnce() -> T>(test_name: &str, make_template: F) {
    let path = PathBuf::from(format!("tests/{}", test_name));

    //static
    let template = make_template();
    let result_path = path.join("static.html");
    if let Err(e) = fs::File::create(&result_path).and_then(|mut file| write!(file, "{}", template)) {
        panic!("couldn't write {}: {}", result_path.display(), e);
    }

    //dynamic
    let values_path = path.join("values.json");
    let values = match fs::File::open(&values_path).map_err(From::from).and_then(|file| serde_json::from_reader(file)) {
        Ok(v) => v,
        Err(e) => panic!("couldn't open and decode {}: {}", values_path.display(), e),
    };

    let template_path = path.join("template.html");
    let mut template_src = String::new();
    if let Err(e) = fs::File::open(&template_path).and_then(|mut file| file.read_to_string(&mut template_src)) {
        panic!("couldn't read {}: {}", template_path.display(), e);
    }

    let parser = Parser::new();
    let mut template = match parser.parse_string(template_src) {
        Ok(t) => t,
        Err(e) => panic!("couldn't parse {}: {}", template_path.display(), e),
    };

    template.content = values;
    let result_path = path.join("dynamic.html");
    if let Err(e) = fs::File::create(&result_path).and_then(|mut file| write!(file, "{}", template)) {
        panic!("couldn't write {}: {}", result_path.display(), e);
    }
}
