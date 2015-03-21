use std::io::{self, Write};

use string_cache::atom::Atom;

macro_rules! line {
    ($writer:ident, $indent:expr, $($format:tt)*) => (
        {
            try!(indent($writer, $indent));
            try!(writeln!($writer, $($format)*));
        }
    )
}

pub enum Token {
    BeginTag(Atom),
    EndTag(bool),
    CloseTag(Atom),
    BeginAttribute(Atom, Content),
    AppendToAttribute(Content),
    EndAttribute,
    BeginText(Content),
    AppendToText(Content),
    EndText,

    If(String),
    EndIf,
}

pub enum Content {
    String(String),
    Placeholder(String)
}

pub fn build_js<W: Write>(w: &mut W, name: &str, tokens: &[Token]) -> io::Result<()> {
    let mut indent = 1;
    let mut var_counter = 0u32;
    let mut element_stack = vec![];
    let mut attribute_var = (Atom::from_slice(""), String::new());
    let mut text_var = String::new();
    line!(w, 0, "{}.prototype.render_to = function(root) {{", name);

    for token in tokens {
        match token {
            &Token::BeginTag(ref name) => {
                let var = format!("tag_{}_{}", to_valid_ident(name.as_slice()), var_counter);
                var_counter += 1;
                line!(w, indent, "var {} = document.createElement(\"{}\");", var, name.as_slice());
                element_stack.push(var);
            },
            &Token::EndTag(self_close) => if self_close {
                if let Some(child) = element_stack.pop() {
                    match element_stack.last() {
                        Some(parent) => line!(w, indent, "{}.appendChild({});", parent, child),
                        None => line!(w, indent, "root.appendChild({});", child)
                    }
                }
            },
            &Token::CloseTag(_) => {
                if let Some(child) = element_stack.pop() {
                    match element_stack.last() {
                        Some(parent) => line!(w, indent, "{}.appendChild({});", parent, child),
                        None => line!(w, indent, "root.appendChild({});", child)
                    }
                }
            },
            &Token::BeginAttribute(ref name, ref content) => {
                attribute_var = (name.clone(), format!("attr_{}_{}", to_valid_ident(name.as_slice()), var_counter));
                var_counter += 1;
                match content {
                    &Content::String(ref content) => line!(w, indent, "var {} = \"{}\";", attribute_var.1, content),
                    &Content::Placeholder(ref placeholder) => {
                        line!(w, indent, "var {};", attribute_var.1);
                        line!(w, indent, "if(this.{} !== null) {{", placeholder);
                        line!(w, indent + 1, "{} = this.{};", attribute_var.1, placeholder);
                        line!(w, indent, "}} else {{");
                        line!(w, indent + 1, "{} = \"\";", attribute_var.1);
                        line!(w, indent, "}}");
                    }
                }
            },
            &Token::AppendToAttribute(ref content) => {
                match content {
                    &Content::String(ref content) => line!(w, indent, "{} += \"{}\";", attribute_var.1, content),
                    &Content::Placeholder(ref placeholder) => {
                        line!(w, indent, "if(this.{} !== null) {{", placeholder);
                        line!(w, indent + 1, "{} += this.{};", attribute_var.1, placeholder);
                        line!(w, indent, "}}");
                    }
                }
            },
            &Token::EndAttribute => {
                if let Some(element) = element_stack.last() {
                    line!(w, indent, "{}.setAttribute(\"{}\", {});", element, attribute_var.0.as_slice(), attribute_var.1);
                }
            },
            &Token::BeginText(ref content) => {
                text_var = format!("text_{}", var_counter);
                var_counter += 1;
                match content {
                    &Content::String(ref content) => line!(w, indent, "var {} = \"{}\";", text_var, content),
                    &Content::Placeholder(ref placeholder) => {
                        line!(w, indent, "var {};", text_var);
                        line!(w, indent, "if(this.{} !== null) {{", placeholder);
                        line!(w, indent + 1, "{} = this.{};", text_var, placeholder);
                        line!(w, indent, "}} else {{");
                        line!(w, indent + 1, "{} = \"\";", text_var);
                        line!(w, indent, "}}");
                    }
                }
            },
            &Token::AppendToText(ref content) => {
                match content {
                    &Content::String(ref content) => line!(w, indent, "{} += \"{}\";", text_var, content),
                    &Content::Placeholder(ref placeholder) => {
                        line!(w, indent, "if(this.{} !== null) {{", placeholder);
                        line!(w, indent + 1, "{} += this.{};", text_var, placeholder);
                        line!(w, indent, "}}");
                    }
                }
            },
            &Token::EndText => {
                if let Some(element) = element_stack.last() {
                    line!(w, indent, "{}.appendChild(document.createTextNode({}));", element, text_var);
                } else {
                    line!(w, indent, "root.appendChild(document.createTextNode({}));", text_var);
                }
            }
            &Token::If(ref cond) => {
                line!(w, indent, "if({}) {{", cond);
                indent += 1;
            },
            &Token::EndIf => {
                indent -= 1;
                line!(w, indent, "}}");
            }
        }
    }

    line!(w, 0, "}};");
    Ok(())
}

pub fn build_rust<W: Write>(w: &mut W, name: &str, lifetime: Option<&str>, tokens: &[Token]) -> io::Result<()> {
    let mut indent = 0;

    if let Some(lifetime) = lifetime {
        line!(w, indent, "impl<{lifetime}> {name}<{lifetime}> {{", lifetime = lifetime, name = name);
    } else {
        line!(w, indent, "impl {} {{", name);
    }
    indent += 1;

    line!(w, indent, "pub fn render_to<W: ::std::io::Write>(&self, writer: &mut W) -> ::std::io::Result<()> {{");
    indent += 1;

    for token in tokens {
        match token {
            &Token::BeginTag(ref name) => line!(w, indent, "try!(write!(writer, \"<{}\"));", name.as_slice()),
            &Token::EndTag(_self_close) => line!(w, indent, "try!(write!(writer, \">\"));"),
            &Token::CloseTag(ref name) => line!(w, indent, "try!(write!(writer, \"</{}>\"));", name.as_slice()),
            &Token::BeginAttribute(ref name, ref content) => match content {
                &Content::String(ref content) => line!(w, indent, "try!(write!(writer, \" {}=\\\"{}\"));", name.as_slice(), content),
                &Content::Placeholder(ref placeholder) => {
                    line!(w, indent, "try!(write!(writer, \" {}=\\\"\"));", name.as_slice());
                    line!(w, indent, "if let Some(val) = self.{} {{", placeholder);
                    line!(w, indent + 1, "try!(write!(writer, \"{{}}\", val));");
                    line!(w, indent, "}}");
                }
            },
            &Token::AppendToAttribute(ref content) => match content {
                &Content::String(ref content) => line!(w, indent, "try!(write!(writer, \"{}\"));", content),
                &Content::Placeholder(ref placeholder) => {
                    line!(w, indent, "if let Some(val) = self.{} {{", placeholder);
                    line!(w, indent + 1, "try!(write!(writer, \"{{}}\", val));");
                    line!(w, indent, "}}");
                }
            },
            &Token::EndAttribute => line!(w, indent, "try!(write!(writer, \"\\\"\"));"),
            &Token::BeginText(ref text) | &Token::AppendToText(ref text) => match text {
                &Content::String(ref text) => line!(w, indent, "try!(write!(writer, \"{}\"));", text),
                &Content::Placeholder(ref placeholder) => {
                    line!(w, indent, "if let Some(val) = self.{} {{", placeholder);
                    line!(w, indent + 1, "try!(write!(writer, \"{{}}\", val));");
                    line!(w, indent, "}}");
                },
            },
            &Token::EndText => {},
            &Token::If(ref cond) => {
                line!(w, indent, "if self.{} {{", cond);
                indent += 1;
            },
            &Token::EndIf => {
                indent -= 1;
                line!(w, indent, "}}");
            }
        }
    }

    line!(w, indent, "Ok(())");
    indent -= 1;

    line!(w, indent, "}}");
    indent -= 1;

    line!(w, indent, "}}");
    Ok(())
}

#[inline]
fn indent<W: Write>(writer: &mut W, steps: u8) -> io::Result<()> {
    for _ in 0..steps {
        try!(write!(writer, "    "));
    }
    Ok(())
}

fn to_valid_ident(name: &str) -> String {
    name.chars().map(|c| if !c.is_alphanumeric() { '_' } else { c }).collect()
}