use std::io::{self, Write};
use std::fmt;

pub use symbiosis_core::codegen::*;

///Shortcut for the commonly used `try!(write!(...))`.
#[macro_export]
macro_rules! try_w {
    ($($args:tt)*) => (try!(write!($($args)*)))
}

///Code generators are used to generate template code in different languages
///and they have to implement this trait.
pub trait Codegen {
    type Error;

    ///Initiate the code writer.
    fn init_writer<'a, W: Write>(&self, w: &'a mut W) -> Writer<'a, W>;

    ///Generate code for a single template.
    fn build_template<W: Write>(&self, w: &mut Writer<W>, name: &str, params: &Params, tokens: &[Token]) -> Result<(), Self::Error>;

    ///Generate code for a module or a similar collection containing multiple templates.
    fn build_module<W, F>(&self, w: &mut Writer<W>, build_templates: F) -> Result<(), Self::Error> where
        W: Write,
        F: FnOnce(&mut Writer<W>) -> Result<(), Self::Error>
    {
        build_templates(w)
    }
}

pub struct Writer<'a, W: Write + 'a> {
    indent_str: &'static str,
    base_indent: u8,
    indent: u8,
    writer: &'a mut W
}

impl<'a, W: Write> Writer<'a, W> {
    pub fn new(writer: &'a mut W, indent: &'static str) -> Writer<'a, W> {
        Writer {
            indent_str: indent,
            base_indent: 0,
            indent: 0,
            writer: writer
        }
    }

    pub fn begin_line<'w>(&'w mut self) -> Line<'w, W> {
        Line {
            writer: self.writer,
            indent: self.base_indent + self.indent,
            indent_str: self.indent_str,
            indent_written: false,
            end_written: false,
        }
    }

    pub fn indented_line<'w>(&'w mut self) -> Line<'w, W> {
        Line {
            writer: self.writer,
            indent: self.base_indent + self.indent + 1,
            indent_str: self.indent_str,
            indent_written: false,
            end_written: false
        }
    }

    pub fn write_fmt(&mut self, args: fmt::Arguments) -> io::Result<()> {
        let mut line = self.begin_line();
        try!(line.write_fmt(args));
        line.end()
    }

    pub fn indent(&mut self) {
        self.indent += 1;
    }

    pub fn unindent(&mut self) {
        if self.indent > 0 {
            self.indent -= 1;
        }
    }

    pub fn block<'w>(&'w mut self) -> Writer<'w, W> {
        Writer {
            indent_str: self.indent_str,
            base_indent: self.base_indent + self.indent + 1,
            indent: 0,
            writer: self.writer
        }
    }
}

pub struct Line<'a, W: Write + 'a> {
    writer: &'a mut W,
    indent: u8,
    indent_str: &'static str,
    indent_written: bool,
    end_written: bool
}

impl<'a, W: Write> Line<'a, W> {
    pub fn write_fmt(&mut self, args: fmt::Arguments) -> io::Result<()> {
        try!(self.write_indent());
        self.writer.write_fmt(args)
    }

    pub fn end(mut self) -> io::Result<()> {
        self.write_end()
    }

    #[inline]
    fn write_indent(&mut self) -> io::Result<()> {
        if !self.indent_written {
            for _ in 0..self.indent {
                try!(self.writer.write_all(self.indent_str.as_bytes()))
            }
            self.indent_written = true;
        }
        Ok(())
    }

    fn write_end(&mut self) -> io::Result<()> {
        if !self.end_written {
            self.end_written = true;
            self.writer.write_all(b"\n")
        } else {
            Ok(())
        }
    }
}

impl<'a, W: Write> Drop for Line<'a, W> {
    #[allow(unused_must_use)]
    fn drop(&mut self) {
        self.write_end();
    }
}