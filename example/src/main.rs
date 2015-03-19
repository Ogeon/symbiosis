mod templates;

use std::io::Write;

fn main() {
    let template = templates::ImgBox;
    let mut out = std::io::stdout();
    template.render_to(&mut out).unwrap();
    out.flush().unwrap();
}
