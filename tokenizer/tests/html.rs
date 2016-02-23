//These tests were "borrowed" from html5ever. Some of them are not applicable,
//since the purpose of Symbiosis is to recreate the original document as
//closely as possible.

extern crate symbiosis_tokenizer;

use std::collections::HashMap;

use symbiosis_tokenizer::{Tokenizer, TokenSink, StrTendril};
use symbiosis_tokenizer::codegen::{Token, Content};
use symbiosis_tokenizer::fragment::{Fragment, FragmentStore};

struct Sink {
    buffer: String,
    fragments: HashMap<&'static str, Box<Fragment>>
}

impl<'a> TokenSink for &'a mut Sink {
    fn process_token(&mut self, token: Token) {
        match token {
            Token::SetDoctype(doctype) => {
                self.buffer.push_str("<!DOCTYPE");
                if let Some(ref name) = doctype.name {
                    self.buffer.push_str(name);
                }

                if let Some(ref public_id) = doctype.public_id {
                    self.buffer.push_str(&format!(" PUBLIC \"{}\"", public_id));
                } else if doctype.system_id.is_some() {
                    self.buffer.push_str(" SYSTEM");
                }

                if let Some(ref system_id) = doctype.system_id {
                    self.buffer.push_str(&format!(" \"{}\"", system_id));
                }
                self.buffer.push_str(">");
            },
            Token::Comment(comment) => self.buffer.push_str(&format!("<!--{}-->", comment)),
            Token::BeginTag(name) => self.buffer.push_str(&format!("<{}", name)),
            Token::BeginAttribute(name, content) => {
                let content = match content {
                    Content::String(s) => s.to_string(),
                    Content::Placeholder(_, _) => "".into(),
                };
                self.buffer.push_str(&format!(" {}=\"{}", name, content));
            },
            Token::Text(content) | Token::AppendToAttribute(content) => {
                let content = match content {
                    Content::String(s) => s.to_string(),
                    Content::Placeholder(_, _) => "".into(),
                };
                self.buffer.push_str(&content);
            },
            Token::EndAttribute => self.buffer.push_str("\""),
            Token::EndTag(self_closing) => if self_closing {
                self.buffer.push_str("/>");
            } else {
                self.buffer.push_str(">");
            },
            Token::CloseTag(name) => self.buffer.push_str(&format!("</{}>", name)),
            Token::Scope(_) | Token::End => {}
        }
    }

    fn fragments(&self) -> &FragmentStore {
        &self.fragments
    }
}

fn parse_and_serialize(input: StrTendril) -> StrTendril {
    let mut sink = Sink {
        buffer: String::new(),
        fragments: HashMap::new(),
    };
    {
        let mut tokenizer = Tokenizer::new(&mut sink);
        tokenizer.parse_string(input.into()).unwrap();
    }
    sink.buffer.into()
}

macro_rules! test {
    ($name:ident, $input:expr, $output:expr) => {
        #[test]
        fn $name() {
            assert_eq!($output, &*parse_and_serialize($input.into()));
        }
    };

    // Shorthand for $output = $input
    ($name:ident, $input:expr) => {
        test!($name, $input, $input);
    };
}

test!(empty, r#""#);
test!(smoke_test, r#"<p><i>Hello</i>, World!</p>"#);

/*test!(misnest, r#"<p><i>Hello!</p>, World!</i>"#,
    r#"<p><i>Hello!</i></p><i>, World!</i>"#);*/

test!(attr_literal, r#"<base foo="<'>">"#);
test!(attr_escape_amp, r#"<base foo="&amp;">"#);
//test!(attr_escape_amp_2, r#"<base foo=&amp>"#, r#"<base foo="&amp;">"#);
test!(attr_escape_nbsp, "<base foo=x\u{a0}y>", r#"<base foo="x&nbsp;y">"#);
test!(attr_escape_quot, r#"<base foo='"'>"#, r#"<base foo="&quot;">"#);
/*est!(attr_escape_several, r#"<span foo=3 title='test "with" &amp;quot;'>"#,
    r#"<span foo="3" title="test &quot;with&quot; &amp;quot;"></span>"#);*/

test!(text_literal, r#"<p>"'"</p>"#);
test!(text_escape_amp, r#"<p>&amp;</p>"#);
//test!(text_escape_amp_2, r#"<p>&amp</p>"#, r#"<p>&amp;</p>"#);
test!(text_escape_nbsp, "<p>x\u{a0}y</p>", r#"<p>x&nbsp;y</p>"#);
test!(text_escape_lt, r#"<p>&lt;</p>"#);
test!(text_escape_gt, r#"<p>&gt;</p>"#);
test!(text_escape_gt2, r#"<p>></p>"#, r#"<p>&gt;</p>"#);

test!(script_literal, r#"<script>(x & 1) < 2; y > "foo" + 'bar'</script>"#);
test!(style_literal, r#"<style>(x & 1) < 2; y > "foo" + 'bar'</style>"#);
test!(xmp_literal, r#"<xmp>(x & 1) < 2; y > "foo" + 'bar'</xmp>"#);
test!(iframe_literal, r#"<iframe>(x & 1) < 2; y > "foo" + 'bar'</iframe>"#);
test!(noembed_literal, r#"<noembed>(x & 1) < 2; y > "foo" + 'bar'</noembed>"#);
test!(noframes_literal, r#"<noframes>(x & 1) < 2; y > "foo" + 'bar'</noframes>"#);

test!(pre_lf_0, "<pre>foo bar</pre>");
//test!(pre_lf_1, "<pre>\nfoo bar</pre>", "<pre>foo bar</pre>");
test!(pre_lf_2, "<pre>\n\nfoo bar</pre>");

test!(textarea_lf_0, "<textarea>foo bar</textarea>");
//test!(textarea_lf_1, "<textarea>\nfoo bar</textarea>", "<textarea>foo bar</textarea>");
test!(textarea_lf_2, "<textarea>\n\nfoo bar</textarea>");

test!(listing_lf_0, "<listing>foo bar</listing>");
//test!(listing_lf_1, "<listing>\nfoo bar</listing>", "<listing>foo bar</listing>");
test!(listing_lf_2, "<listing>\n\nfoo bar</listing>");

test!(comment_1, r#"<p>hi <!--world--></p>"#);
test!(comment_2, r#"<p>hi <!-- world--></p>"#);
test!(comment_3, r#"<p>hi <!--world --></p>"#);
test!(comment_4, r#"<p>hi <!-- world --></p>"#);

test!(attr_ns_1, r#"<svg xmlns="bleh"></svg>"#);
test!(attr_ns_2, r#"<svg xmlns:foo="bleh"></svg>"#);
test!(attr_ns_3, r#"<svg xmlns:xlink="bleh"></svg>"#);
test!(attr_ns_4, r#"<svg xlink:href="bleh"></svg>"#);