//!The common traits and types for Symbiosis templates.

use std::io::{self, Write};

///Common trait for Symbiosis templates.
pub trait Template {
    fn render_to(&self, writer: &mut Write) -> io::Result<()>;
}