extern crate symbiosis_rust;

use symbiosis_rust::Template;

mod templates;

use std::io::Write;

fn main() {
    let template = templates::ImgBox {
        url: "http://example.com/cat.gif",
        subject: Some("a cat")
    };
    let mut out = std::io::stdout();
    template.render_to(&mut out).unwrap();
    out.flush().unwrap();

    let template = templates::ImgBox {
        url: "http://example.com/abstract.png",
        subject: None
    };
    let mut out = std::io::stdout();
    template.render_to(&mut out).unwrap();
    out.flush().unwrap();
}
