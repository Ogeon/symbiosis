use std::borrow::Cow;
use std::fmt;

use tendril::StrTendril;

use codegen::{Scope, Logic, ContentType};

#[macro_use]
pub mod pattern;

macro_rules! impl_fragment {
    ($(#[$attr:meta])* frag $str_name:expr => $name:ident: |$args:ident| $process:block) => {
        impl_fragment!($(#[$attr])* frag $str_name => $name: |$args: [input, 0]| $process);
    };
    ($(#[$attr:meta])* frag $str_name:expr => $name:ident: |$args:ident: [input, $at_least: expr]| $process:block) => {
        $(#[$attr])*
        pub struct $name;

        impl $crate::fragment::Fragment for $name {
            fn identifier(&self) -> &'static str {
                $str_name
            }

            fn pattern(&self) -> $crate::fragment::pattern::Pattern {
                build_pattern!([input](",", $at_least))
            }

            fn process(&self, args: Vec<$crate::fragment::pattern::Argument>)
            -> Result<$crate::fragment::ReturnType, $crate::fragment::Error> {
                let $args: Vec<InputType> = args.into_iter()
                    .nth(0)
                    .map(|a| match a.into_repeat() {
                        Ok(i) => i,
                        Err(e) => panic!("expected input, but found {:?}", e)
                    })
                    .expect("expected repeated input, but found nothing")
                    .into_iter()
                    .map(|a| a.into_iter().nth(0).expect("found empty patterns where input was expected"))
                    .map(|a| match a.into_input() {
                        Ok(i) => i,
                        Err(e) => panic!("expected input, but found {:?}", e)
                    })
                    .collect();
                $process
            }
        }
    };
    ($(#[$attr:meta])* pattern ($arg_t: ident { $($pattern: tt)* }) frag $str_name:expr => $name:ident: |$args:ident| $process:block) => {
        $(#[$attr])*
        pub struct $name;

        impl $crate::fragment::Fragment for $name {
            fn identifier(&self) -> &'static str {
                $str_name
            }

            fn pattern(&self) -> $crate::fragment::pattern::Pattern {
                build_annotated_pattern!($arg_t { $($pattern)* })
            }

            fn process(&self, args: Vec<$crate::fragment::pattern::Argument>)
            -> Result<$crate::fragment::ReturnType, $crate::fragment::Error>{
                pattern_decoder!($arg_t { $($pattern)* });
                let $args = try!($arg_t::decode(args));
                $process
            }
        }
    };
    ($(#[$attr:meta])* pattern ($($pattern: tt)*) frag $str_name:expr => $name:ident: |$args:ident| $process:block) => {
        $(#[$attr])*
        pub struct $name;

        impl $crate::fragment::Fragment for $name {
            fn identifier(&self) -> &'static str {
                $str_name
            }

            fn pattern(&self) -> $crate::fragment::pattern::Pattern {
                build_pattern!($($pattern)*)
            }

            fn process(&self, $args: Vec<$crate::fragment::pattern::Argument>)
            -> Result<$crate::fragment::ReturnType, $crate::fragment::Error>
            $process
        }
    }
}

#[derive(Debug)]
pub struct Error {
    callstack: Vec<Cow<'static, str>>,
    kind: ErrorKind
}

impl Error {
    pub fn expected_input(but_found: Option<pattern::Argument>) -> Error {
        Error {
            callstack: vec![],
            kind: ErrorKind::UnexpectedArgument(ArgumentKind::Input, but_found)
        }
    }

    pub fn expected_optional(but_found: Option<pattern::Argument>) -> Error {
        Error {
            callstack: vec![],
            kind: ErrorKind::UnexpectedArgument(ArgumentKind::Optional, but_found)
        }
    }
    
    pub fn expected_repeating(but_found: Option<pattern::Argument>) -> Error {
        Error {
            callstack: vec![],
            kind: ErrorKind::UnexpectedArgument(ArgumentKind::Repeated, but_found)
        }
    }

    pub fn unexpected_input_type(expected: InputType, but_found: InputType) -> Error {
        Error {
            callstack: vec![],
            kind: ErrorKind::UnexpectedInputType(expected, but_found)
        }
    }

    pub fn other(error: Cow<'static, str>) -> Error {
        Error {
            callstack: vec![],
            kind: ErrorKind::Other(error)
        }
    }

    pub fn add_callee<T: Into<Cow<'static, str>>>(&mut self, callee: T) {
        self.callstack.push(callee.into())
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.callstack.len() > 0 {
            try!(f.write_str("error when calling "));
            for (i, fragment) in self.callstack.iter().enumerate() {
                if i > 0 {
                    try!(f.write_str(" in "));
                }
                try!(f.write_str(fragment));
            }
            try!(f.write_str(": "));
        }

        self.kind.fmt(f)
    }
}

impl<T: Into<Cow<'static, str>>> From<T> for Error {
    fn from(error: T) -> Error {
        Error::other(error.into())
    }
}

#[derive(Debug)]
enum ArgumentKind {
    Input,
    Optional,
    Repeated
}

impl fmt::Display for ArgumentKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ArgumentKind::Input => f.write_str("input"),
            ArgumentKind::Optional => f.write_str("an optional pattern"),
            ArgumentKind::Repeated => f.write_str("a repeated pattern")
        }
    }
}

#[derive(Debug)]
enum ErrorKind {
    UnexpectedArgument(ArgumentKind, Option<pattern::Argument>),
    UnexpectedInputType(InputType, InputType),
    Other(Cow<'static, str>)
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ErrorKind::UnexpectedArgument(ref expected, Some(ref arg)) => write!(f, "expected {}, but found {:?}", expected, arg),
            ErrorKind::UnexpectedArgument(ref expected, None) => write!(f, "expected {}, but found nothing more", expected),
            ErrorKind::UnexpectedInputType(ref expected, ref found) => write!(f, "expected {:?}, but found {:?}", expected, found),
            ErrorKind::Other(ref e) => e.fmt(f)
        }
    }
}

pub trait Fragment {
    fn identifier(&self) -> &'static str;
    fn pattern(&self) -> pattern::Pattern;
    fn process(&self, args: Vec<pattern::Argument>) -> Result<ReturnType, Error>;
}

impl<F: Fn(Vec<pattern::Argument>) -> Result<ReturnType, Error>> Fragment for (&'static str, F) {
    fn identifier(&self) -> &'static str {
        self.0
    }

    fn pattern(&self) -> pattern::Pattern {
        build_pattern!([input](","))
    }

    fn process(&self, args: Vec<pattern::Argument>) -> Result<ReturnType, Error> {
        self.1(args)
    }
}

impl<'a, F: Fragment> Fragment for &'a F {
    fn identifier(&self) -> &'static str {
        (*self).identifier()
    }

    fn pattern(&self) -> pattern::Pattern {
        (*self).pattern()
    }

    fn process(&self, args: Vec<pattern::Argument>) -> Result<ReturnType, Error> {
        (*self).process(args)
    }
}

///Things that can be returned from fragments.
pub enum ReturnType {
    ///A plane text string.
    String(StrTendril),
    ///A placeholder parameter and its preferred content type.
    Placeholder(StrTendril, ContentType),
    ///A logic expression.
    Logic(Logic),
    ///The beginning of a scope.
    Scope(Scope),
    ///The end of a scope.
    End
}

///Things that can be sent into fragments.
pub enum InputType {
    ///A placeholder parameter and its preferred content type.
    Placeholder(StrTendril, ContentType),
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


impl_fragment!{
    #[doc = "Start of an `if` scope."]
    #[doc = ""]
    #[doc = "Anything between `{{ if(...) }}` and `{{ end }}` will"]
    #[doc = "be hidden if the condition is false. `if(a, b, ...)` will be interpreted as"]
    #[doc = "`if(and(a, b, ...))`."]
    #[doc = ""]
    #[doc = "```text"]
    #[doc = "{{ if(name) }}<p>My name is {{ name }}</p>{{ end }}"]
    #[doc = "{{ if(name, age) }}<p>My name is {{ name }} and I am {{ age }} years old.</p>{{ end }}"]
    #[doc = "{{ if(am_a_teapot) }}<p>I am a teapot</p>{{ end }}"]
    #[doc = "```"]
    frag "if" => If: |args: [input, 1]| {
        if args.len() == 0 {
            return Err("if requires at least one argument".into());
        }

        let mut conds = Vec::with_capacity(args.len());

        for arg in args {
            match arg {
                InputType::Placeholder(name, _) => conds.push(Logic::Value(name)),
                InputType::Logic(cond) => conds.push(cond)
            }
        }

        Ok(ReturnType::Scope(Scope::If(Logic::And(conds))))
    }
}

impl_fragment!{
    #[doc = "`and` logic fragment."]
    #[doc = ""]
    #[doc = "`and(a, b, ...)` will only be true if all of the"]
    #[doc = "conditions `a, b, ...` are true."]
    frag "and" => And: |args: [input, 1]| {
        if args.len() == 0 {
            return Err("and requires at least one argument".into());
        }

        let mut conds = Vec::with_capacity(args.len());

        for arg in args {
            match arg {
                InputType::Placeholder(name, _) => conds.push(Logic::Value(name)),
                InputType::Logic(cond) => conds.push(cond)
            }
        }

        Ok(ReturnType::Logic(Logic::And(conds)))
    }
}

impl_fragment!{
    #[doc = "`or` logic fragment."]
    #[doc = ""]
    #[doc = "`or(a, b, ...)` will be true if one of the conditions"]
    #[doc = "`a, b, ...` is true."]
    frag "or" => Or: |args: [input, 1]| {
        if args.len() == 0 {
            return Err("or requires at least one argument".into());
        }

        let mut conds = Vec::with_capacity(args.len());

        for arg in args {
            match arg {
                InputType::Placeholder(name, _) => conds.push(Logic::Value(name)),
                InputType::Logic(cond) => conds.push(cond)
            }
        }

        Ok(ReturnType::Logic(Logic::Or(conds)))
    }
}

impl_fragment!{
    #[doc = "`not` logic fragment."]
    #[doc = ""]
    #[doc = "`not(a)` will be true if condition `a` is false."]
    pattern (NotArgs { cond: input })
    frag "not" => Not: |args| {
        let cond = match args.cond {
            InputType::Placeholder(name, _) => Logic::Value(name),
            InputType::Logic(cond) => cond
        };

        Ok(ReturnType::Logic(Logic::Not(Box::new(cond))))
    }
}

impl_fragment!{
    #[doc = "`template(a)` tells the parser that the placeholder `a` is an other template."]
    pattern (TemplateArgs { template: input })
    frag "template" => Template: |args| {
        match args.template {
            InputType::Placeholder(name, _) => Ok(ReturnType::Placeholder(name, ContentType::Template(false))),
            e => return Err(Error::unexpected_input_type(InputType::Placeholder("template".into(), ContentType::Template(false)), e))
        }
    }
}

impl_fragment!{
    #[doc = "Start of a `foreach` scope."]
    #[doc = ""]
    #[doc = "`foreach(element in collection)` or `foreach(key => element in collection)` will repeat"]
    #[doc = "everything within the scope for each `element` (and `key`) in `collection`."]
    pattern (ForEachArgs {
        key: (Key { k: input {"=>"} })?
        element: input
        {"in"}
        collection: input
    })
    frag "foreach" => ForEach: |args| {
        match (args.key, args.element, args.collection) {
            (None, element, collection) => {
                let element = match element {
                    InputType::Placeholder(name, _) => name,
                    e => return Err(Error::unexpected_input_type(InputType::Placeholder("element".into(), ContentType::String(false)), e))
                };

                let collection = match collection {
                    InputType::Placeholder(name, _) => name,
                    e => return Err(Error::unexpected_input_type(InputType::Placeholder("collection".into(), ContentType::Collection(None, false)), e))
                };

                Ok(ReturnType::Scope(Scope::ForEach(collection, element, None)))
            },
            (Some(Key{ k: key }), element, collection) => {
                let key = match key {
                    InputType::Placeholder(name, _) => name,
                    e => return Err(Error::unexpected_input_type(InputType::Placeholder("key".into(), ContentType::String(false)), e))
                };

                let element = match element {
                    InputType::Placeholder(name, _) => name,
                    e => return Err(Error::unexpected_input_type(InputType::Placeholder("element".into(), ContentType::String(false)), e))
                };

                let collection = match collection {
                    InputType::Placeholder(name, _) => name,
                    e => return Err(Error::unexpected_input_type(InputType::Placeholder("collection".into(), ContentType::Collection(None, false)), e))
                };

                Ok(ReturnType::Scope(Scope::ForEach(collection, element, Some(key))))
            }
        }
    }
}