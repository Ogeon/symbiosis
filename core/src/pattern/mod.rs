use lalrpop_util::ParseError as LalrpopError;

mod grammar;

pub type ParseError<'a> = LalrpopError<usize, (usize, &'a str), ()>;

pub struct AnnotatedPattern {
    pub name: Option<String>,
    pub components: Vec<AnnotatedComponent>,
}

impl AnnotatedPattern {
    pub fn from_str(string: &str) -> Result<AnnotatedPattern, ParseError> {
        self::grammar::parse_Pattern(string)
    }
}

pub struct AnnotatedComponent {
    pub name: Option<String>,
    pub kind: ComponentKind,
}

///A pattern component.
pub enum ComponentKind {
    ///Expect an arbitrary piece of text.
    Token(String),
    ///Expect a repeated token. It must repeat at leas `at_least` times and be separated by `delimiter`.
    Repeat {
        at_least: usize,
        delimiter: String,
        pattern: AnnotatedPattern
    },
    ///Expect an optional pattern.
    Optional(AnnotatedPattern),
    ///Expect an input.
    Input,
    ///Expect a string literal.
    String,
}
