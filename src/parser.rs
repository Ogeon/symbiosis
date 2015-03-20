pub enum Token {
	Text(String)
}

pub fn parse_content(content: &str) -> Vec<Token> {
	let mut buf = String::new();
	let mut tokens = Vec::new();
	let mut content = content.chars();

	loop {
		match content.next() {
			Some('\n') => buf.push_str("\\n"),
			Some('\t') => buf.push_str("\\t"),
			Some(c) => buf.push(c),
			None => break
		}
	}

	if buf.len() > 0 {
		tokens.push(Token::Text(buf));
	}

	tokens
}