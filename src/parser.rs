use std::str::Chars;

pub enum Token {
    Text(String),
    Placeholder(String)
}

pub fn parse_content(content: &str) -> Vec<Token> {
    let mut buf = String::new();
    let mut tokens = Vec::new();
    let mut content = content.chars();

    while let Some(character) = content.next() {
        match character {
            '{' => match content.next() {
                Some('{') => {
                    if buf.len() > 0 {
                        tokens.push(Token::Text(buf));
                        buf = String::new();
                    }

                    tokens.push(parse_fragment(&mut content));
                },
                Some(c) => {
                    buf.push('{');
                    buf.push(c);
                },
                None => break
            },
            '\n' => buf.push_str("\\n"),
            '\t' => buf.push_str("\\t"),
            c => buf.push(c)
        }
    }

    if buf.len() > 0 {
        tokens.push(Token::Text(buf));
    }

    tokens
}

fn parse_fragment(content: &mut Chars) -> Token {
    let mut buf = String::new();
    while let Some(character) = content.next() {
        match character {
            '}' => match content.next() {
                Some('}') | None => break,
                Some(c) => {
                    buf.push('}');
                    buf.push(c);
                }
            },
            c => buf.push(c)
        }
    }

    Token::Placeholder(buf)
}