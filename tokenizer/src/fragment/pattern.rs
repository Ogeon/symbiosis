use std::borrow::Cow;
use std::fmt;

use StrTendril;

use parser::{Input, FragmentKind};
use codegen::{ContentType, Logic, Path};

use symbiosis_core::pattern::{ParseError, AnnotatedPattern, AnnotatedComponent, ComponentKind};

///An input pattern, represented as a sequence of components.
pub struct Pattern(Vec<Component>);

impl Pattern {
    ///Create an empty pattern.
    pub fn new() -> Pattern {
        Pattern(Vec::new())
    }

    pub fn from_str(string: &str) -> Result<Pattern, ParseError> {
        AnnotatedPattern::from_str(string).map(From::from)
    }

    ///Add a new component to the pattern.
    pub fn push(&mut self, component: Component) {
        self.0.push(component);
    }

    #[doc(hidden)]
    pub fn parse<F, E>(&self, src: &[Input], mut get_input: F) -> Result<Vec<Argument>, E> where
        F: FnMut(&FragmentKind) -> Result<InputType, E>,
        E: From<&'static str> + From<String>,
    {
        let mut src = InputSlicer::new(Cow::Borrowed(src));
        self.parse_using(&mut src, &mut get_input)
    }

    fn parse_using<F, E>(&self, src: &mut InputSlicer, get_input: &mut F) -> Result<Vec<Argument>, E> where
        F: FnMut(&FragmentKind) -> Result<InputType, E>,
        E: From<&'static str> + From<String>,
    {
        let mut result = vec![];
        let mut optionals = vec![];

        while result.len() < self.0.len() {
            for component in &self.0[result.len()..] {
                let res = match *component {
                    Component::Token(ref t) => src.expect_token_str(t).map(Argument::Token).ok_or_else(|| format!("expected '{}'", t).into()),
                    Component::Input => src.expect_input().ok_or_else(|| "expected input".into()).and_then(&mut *get_input).map(Argument::Input),
                    Component::String => src.expect_string().map(Argument::String).ok_or_else(|| "expected a string".into()),
                    Component::Repeat { at_least, ref delimiter, ref pattern } => {
                        let mut repeats = Vec::new();
                        let mut snapshot = src.offset();
                        let mut error = None;

                        loop {
                            match pattern.parse_using(src, get_input) {
                                Ok(res) => {
                                    repeats.push(res);
                                    snapshot = src.offset();
                                    if src.expect_token_str(delimiter).is_none() {
                                        src.jump_to(&snapshot);
                                        break;
                                    }
                                },
                                Err(e) => {
                                    error = Some(e);
                                    src.jump_to(&snapshot);
                                    break
                                }
                            }
                        }

                        if repeats.len() >= at_least {
                            Ok(Argument::Repeat(repeats))
                        } else {
                            if let Some(e) = error {
                                Err(e)
                            } else {
                                Err(format!("expected at least {} repetitions, but found {}", at_least, repeats.len()).into())
                            }
                        }
                    },
                    Component::Optional(ref pattern) => {
                        optionals.push((result.len(), src.offset()));
                        pattern.parse_using(src, get_input).map(|r| Argument::Optional(Some(r)))
                    },
                };

                match res {
                    Ok(res) => {
                        result.push(res);
                    },
                    Err(e) => if let Some((old_len, old_offset)) = optionals.pop() {
                        result.truncate(old_len);
                        result.push(Argument::Optional(None));
                        src.jump_to(&old_offset);
                        break;
                    } else {
                        return Err(e);
                    }
                }
            }
        }

        Ok(result)
    }
}

impl From<AnnotatedPattern> for Pattern {
    fn from(pattern: AnnotatedPattern) -> Pattern {
        Pattern(pattern.components.into_iter().map(From::from).collect())
    }
}

///A pattern component.
pub enum Component {
    ///Expect an arbitrary piece of text.
    Token(Cow<'static, str>),
    ///Expect a repeated token. It must repeat at leas `at_lest` times and be separated by `delimiter`.
    Repeat {
        at_least: usize,
        delimiter: Cow<'static, str>,
        pattern: Pattern
    },
    ///Expect an optional pattern.
    Optional(Pattern),
    ///Expect an input.
    Input,
    ///Expect a string litteral.
    String,
}

impl From<AnnotatedComponent> for Component {
    fn from(component: AnnotatedComponent) -> Component {
        component.kind.into()
    }
}

impl From<ComponentKind> for Component {
    fn from(component: ComponentKind) -> Component {
        match component {
            ComponentKind::Token(t) => Component::Token(t.into()),
            ComponentKind::Repeat { at_least, delimiter, pattern } => Component::Repeat {
                at_least: at_least,
                delimiter: delimiter.into(),
                pattern: pattern.into(),
            },
            ComponentKind::Optional(p) => Component::Optional(p.into()),
            ComponentKind::Input => Component::Input,
            ComponentKind::String => Component::String,
        }
    }
}

///The output from a parsed `Pattern`.
///
///See `Component` for details.
pub enum Argument {
    Token(StrTendril),
    Repeat(Vec<Vec<Argument>>),
    Optional(Option<Vec<Argument>>),
    Input(InputType),
    String(StrTendril),
}

///Things that can be sent into fragments.
pub enum InputType {
    ///A placeholder parameter and its preferred content type.
    Placeholder(Path, ContentType),
    ///A logic expression.
    Logic(Logic)
}

impl fmt::Debug for InputType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            InputType::Placeholder(ref name, ref ty) => write!(f, "placeholder '{}' ({})", name, ty),
            InputType::Logic(_) => f.write_str("a logic expression")
        }
    }
}

impl Argument {
    pub fn into_token(self) -> Result<StrTendril, Argument> {
        match self {
            Argument::Token(t) => Ok(t),
            other => Err(other)
        }
    }

    pub fn into_repeat(self) -> Result<Vec<Vec<Argument>>, Argument> {
        match self {
            Argument::Repeat(r) => Ok(r),
            other => Err(other)
        }
    }

    pub fn into_optional(self) -> Result<Option<Vec<Argument>>, Argument> {
        match self {
            Argument::Optional(o) => Ok(o),
            other => Err(other)
        }
    }

    pub fn into_input(self) -> Result<InputType, Argument> {
        match self {
            Argument::Input(i) => Ok(i),
            other => Err(other)
        }
    }

    pub fn into_string(self) -> Result<StrTendril, Argument> {
        match self {
            Argument::String(s) => Ok(s),
            other => Err(other)
        }
    }
}

impl fmt::Debug for Argument {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Argument::Token(ref t) => write!(f, "token '{}'", t),
            Argument::Repeat(ref r) => r.fmt(f),
            Argument::Optional(None) => "(opt) nothing".fmt(f),
            Argument::Optional(Some(ref p)) => write!(f, "(opt) {:?}", p),
            Argument::Input(ref i) => i.fmt(f),
            Argument::String(ref s) => s.fmt(f),
        }
    }
}

struct InputSlicer<'a> {
    input: Cow<'a, [Input]>,
    offset: usize,
    sub_slicer: Option<Box<InputSlicer<'a>>>,
}

impl<'a> InputSlicer<'a> {
    fn new(input: Cow<'a, [Input]>) -> InputSlicer {
        InputSlicer {
            input: input,
            offset: 0,
            sub_slicer: None,
        }
    }

    fn is_empty(&self) -> bool {
        if let Some(ref sub) = self.sub_slicer {
            sub.is_empty() && self.offset >= self.input.len()
        } else {
            self.offset >= self.input.len()
        }
    }

    fn next(&mut self) -> Option<&Input> {
        let incr_self = if let Some(ref sub) = self.sub_slicer {
            sub.is_empty()
        } else {
            true
        };

        if incr_self {
            self.offset += 1;
            self.sub_slicer = None;
            self.input.get(self.offset - 1)
        } else {
            self.sub_slicer.as_mut().and_then(|s| s.next())
        }
    }

    fn next_token(&mut self) -> Option<&Input> {
        let incr_self = if let Some(ref sub) = self.sub_slicer {
            sub.is_empty()
        } else {
            true
        };

        if incr_self {
            self.offset += 1;
            self.sub_slicer = None;
            if let Some(&Input::Fragment(ref f)) = self.input.get(self.offset - 1) {
                self.sub_slicer = Some(Box::new(InputSlicer::new(Cow::Owned(deconstruct_fragment(f)))));
                self.sub_slicer.as_mut().and_then(|s| s.next_token())
            } else {
                self.input.get(self.offset - 1)
            }
        } else {
            self.sub_slicer.as_mut().and_then(|s| s.next_token())
        }
    }

    fn expect_token_str(&mut self, token: &str) -> Option<StrTendril> {
        match self.next_token() {
            Some(&Input::String(_)) => None,
            Some(&Input::Other(ref s)) => if &**s == token {
                Some(s.clone())
            } else {
                None
            },
            Some(&Input::Fragment(_)) => None,
            None => None
        }
    }

    fn expect_input(&mut self) -> Option<&FragmentKind> {
        if let Some(&Input::Fragment(ref fragment)) = self.next() {
            Some(fragment)
        } else {
            None
        }
    }

    fn expect_string(&mut self) -> Option<StrTendril> {
        if let Some(&Input::String(ref s)) = self.next() {
            Some(s.clone())
        } else {
            None
        }
    }

    fn jump_to(&mut self, indices: &[usize]) {
        let mut current = Some(self);
        let mut indices = indices.iter();
        let mut index = indices.next();
        while let (Some(slicer), Some(&i)) = (current.take(), index.take()) {
            slicer.offset = i;

            if let (Some(next), Some(&Input::Fragment(ref f))) = (indices.next(), slicer.input.get(slicer.offset)) {
                slicer.sub_slicer = Some(Box::new(InputSlicer::new(Cow::Owned(deconstruct_fragment(f)))));
                current = slicer.sub_slicer.as_mut().map(|s| &mut **s);
                index = Some(next);
            }
        }
    }

    fn offset(&self) -> Vec<usize> {
        let mut offsets = vec![];
        let mut current = Some(self);
        while let Some(slicer) = current.take() {
            offsets.push(slicer.offset);
            current = slicer.sub_slicer.as_ref().map(|s| &**s);
        }

        offsets
    }
}

fn deconstruct_fragment(fragment: &FragmentKind) -> Vec<Input> {
    match *fragment {
        FragmentKind::Function(ref name, ref args) => {
            let mut input = vec![Input::Other(name.clone()), Input::Other("(".into())];
            input.extend(args.iter().cloned());
            input.push(Input::Other(")".into()));
            input
        },
        FragmentKind::Placeholder(ref path) => {
            let mut path = path.iter();
            let mut input = path.next().map(|s| vec![Input::Other(s.clone())]).unwrap_or_default();
            let dot = StrTendril::from(".");

            for s in path {
                input.push(Input::Other(dot.clone()));
                input.push(Input::Other(s.clone()));
            }

            input
        }
    }
}
