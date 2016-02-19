use std::borrow::Cow;
use std::fmt;

use tendril::StrTendril;

use fragment::{InputType, Error};
use parser::{Input, FragmentKind};

///Tool for building input patterns.
///
///#Syntax
///
///A pattern consists of four types of items with the following syntax:
///
/// * `input` - expect an input parameter.
/// * `(...)?` - an optional pattern. Ex: `(input)?` for an optional input.
/// * `[...](delimiter)` - expect a repeated pattern of 0 or more repetitions, separated by `delimiter`. Ex: `[input](",")` for a comma separated input list.
/// * `[...](delimiter, n)` - same as above, but with `n` or more repetitions.
/// * `{token}` - expect an arbitrary string `token`. Ex: `input {"or"} input` matches the string `a or b`.
///
///These are combined to form input patterns, like in this example from
///`foreach`:
///
///```
///# #[macro_use] extern crate symbiosis;
///# fn main() {
///let pattern = build_pattern!((input {"=>"})? input {"in"} input);
///# }
///```
///
///This results in the optional `key =>` part, followed by a mandatory
///`element in collection` part.
#[macro_export]
macro_rules! build_pattern {
    ($($pattern: tt)*) => ({
        let mut pattern = $crate::fragment::pattern::Pattern::new();
        __symbiosis_build_pattern_internal!(pattern, $($pattern)*);
        pattern
    })
}

#[macro_export]
#[doc(hidden)]
macro_rules! __symbiosis_build_pattern_internal {
    ($pattern: ident, input $($rest: tt)*) => ({
        $pattern.push($crate::fragment::pattern::Component::Input);
        __symbiosis_build_pattern_internal!($pattern, $($rest)*);
    });
    ($pattern: ident, string $($rest: tt)*) => ({
        $pattern.push($crate::fragment::pattern::Component::String);
        __symbiosis_build_pattern_internal!($pattern, $($rest)*);
    });
    ($pattern: ident, ($($inner: tt)*) ? $($rest: tt)*) => ({
        let mut inner_pattern = $crate::fragment::pattern::Pattern::new();
        __symbiosis_build_pattern_internal!(inner_pattern, $($inner)*);
        $pattern.push($crate::fragment::pattern::Component::Optional(inner_pattern));
        __symbiosis_build_pattern_internal!($pattern, $($rest)*);
    });
    ($pattern: ident, [$($inner: tt)*] ($delimiter: expr, $at_least: expr) $($rest: tt)*) => ({
        let mut inner_pattern = $crate::fragment::pattern::Pattern::new();
        __symbiosis_build_pattern_internal!(inner_pattern, $($inner)*);
        $pattern.push($crate::fragment::pattern::Component::Repeat {
            at_least: $at_least,
            delimiter: ($delimiter).into(),
            pattern: inner_pattern
        });
        __symbiosis_build_pattern_internal!($pattern, $($rest)*);
    });
    ($pattern: ident, [$($inner: tt)*] ($delimiter: expr) $($rest: tt)*) => ({
        let mut inner_pattern = $crate::fragment::pattern::Pattern::new();
        __symbiosis_build_pattern_internal!(inner_pattern, $($inner)*);
        $pattern.push($crate::fragment::pattern::Component::Repeat {
            at_least: 0,
            delimiter: ($delimiter).into(),
            pattern: inner_pattern
        });
        __symbiosis_build_pattern_internal!($pattern, $($rest)*);
    });
    ($pattern: ident, {$token: expr} $($rest: tt)*) => ({
        $pattern.push($crate::fragment::pattern::Component::Token(($token).into()));
        __symbiosis_build_pattern_internal!($pattern, $($rest)*);
    });
    ($pattern: ident,) => ();
}

///The same as `build_pattern!`, but for annotated patterns.
///
///```
///# #[macro_use] extern crate symbiosis;
///# fn main() {
///let pattern = build_annotated_pattern!(ForEach {
///    key: (Key { k: input {"=>"} })? 
///    element: input
///    {"in"}
///     collection: input
///});
///# }
///```
///
///See `pattern_decoder!` for information about annotation syntax.
#[macro_export]
macro_rules! build_annotated_pattern {
    ($name: ident { $($pattern: tt)* }) => ({
        let mut pattern = $crate::fragment::pattern::Pattern::new();
        __symbiosis_build_annotated_pattern_internal!(pattern, $($pattern)*);
        pattern
    })
}

#[macro_export]
#[doc(hidden)]
macro_rules! __symbiosis_build_annotated_pattern_internal {
    ($pattern: ident, {$token: expr} $($rest: tt)*) => ({
        $pattern.push($crate::fragment::pattern::Component::Token(($token).into()));
        __symbiosis_build_annotated_pattern_internal!($pattern, $($rest)*);
    });
    ($pattern: ident, $field: ident : input $($rest: tt)*) => ({
        $pattern.push($crate::fragment::pattern::Component::Input);
        __symbiosis_build_annotated_pattern_internal!($pattern, $($rest)*);
    });
    ($pattern: ident, $field: ident : string $($rest: tt)*) => ({
        $pattern.push($crate::fragment::pattern::Component::String);
        __symbiosis_build_annotated_pattern_internal!($pattern, $($rest)*);
    });
    ($pattern: ident, $field: ident : ($ty: ident { $($inner: tt)* }) ? $($rest: tt)*) => ({
        let mut inner_pattern = $crate::fragment::pattern::Pattern::new();
        __symbiosis_build_annotated_pattern_internal!(inner_pattern, $($inner)*);
        $pattern.push($crate::fragment::pattern::Component::Optional(inner_pattern));
        __symbiosis_build_annotated_pattern_internal!($pattern, $($rest)*);
    });
    ($pattern: ident, $field: ident : [$ty: ident { $($inner: tt)* }] ($delimiter: expr, $at_least: expr) $($rest: tt)*) => ({
        let mut inner_pattern = $crate::fragment::pattern::Pattern::new();
        __symbiosis_build_annotated_pattern_internal!(inner_pattern, $($inner)*);
        $pattern.push($crate::fragment::pattern::Component::Repeat {
            at_least: $at_least,
            delimiter: ($delimiter).into(),
            pattern: inner_pattern
        });
        __symbiosis_build_annotated_pattern_internal!($pattern, $($rest)*);
    });
    ($pattern: ident, $field: ident : [$ty: ident { $($inner: tt)* }] ($delimiter: expr) $($rest: tt)*) => ({
        let mut inner_pattern = $crate::fragment::pattern::Pattern::new();
        __symbiosis_build_annotated_pattern_internal!(inner_pattern, $($inner)*);
        $pattern.push($crate::fragment::pattern::Component::Repeat {
            at_least: 0,
            delimiter: ($delimiter).into(),
            pattern: inner_pattern
        });
        __symbiosis_build_annotated_pattern_internal!($pattern, $($rest)*);
    });
    ($pattern: ident,) => ();
}

///Decodes a pattern result vector into structs, using an annotated pattern.
///
///The annotation is very similar to struct definitions, where each pattern
///component, except arbitrary tokens, is preceded by a field name and the
///component itself works as a type. The structures are defined recursively
///and each substructure must have a unique name. This is a limitation in the
///macro system.
///
///The annotation syntax is:
///
/// * `pattern_decoder!(MainType { ... })` - declares the name of our main result type, here called `MainType`.
/// * `field_name: input` - expect an input and store it as `InputType` in `field_name`.
/// * `field_name: (Inner { ... })?` - expect an optional pattern and store it in `field_name` as `Option<Inner>`.
/// * `field_name: [Inner { ... }](delimiter, n)` - expect a repeated pattern and store it in `field_name` as `Vec<Inner>`.
/// * `field_name: [Inner { ... }](delimiter)` - same as above, but with `n = 0`.
/// * `{token}` - expect an arbitrary string `token`, that will be ignored when decoding.
///
///```
///# #[macro_use] extern crate symbiosis;
///# use std::borrow::Cow;
///# fn main() {}
///# fn ex() -> Result<(), symbiosis::fragment::Error> {
///# let args = Vec::new();
///pattern_decoder!(ForEach {
///    key: (Key { key: input {"=>"} })?
///    element: input
///    {"in"}
///     collection: input
///});
///
///let res = try!(ForEach::decode(args));
///
///match (res.key, res.element, res.collection) {
///    (Some(Key { key }), element, collection) => {
///        println!("key: {:?}, element: {:?}, collection: {:?}", key, element, collection);
///    },
///    (None, element, collection) => {
///        println!("element: {:?}, collection: {:?}", element, collection);
///    }
///}
///# Ok(())
///# }
///```
///
///```
///# #[macro_use] extern crate symbiosis;
///# use std::borrow::Cow;
///# fn main() {}
///# fn ex() -> Result<(), symbiosis::fragment::Error> {
///# let args = Vec::new();
///pattern_decoder!(HeadTail {
///    head: input
///    tail: [Tail { input: input }]("")
///});
///
///let res = try!(HeadTail::decode(args));
///
///println!("head: {:?}, tail length: {}", res.head, res.tail.len());
///# Ok(())
///# }
///```
#[macro_export]
macro_rules! pattern_decoder {
    ($name: ident { $($pattern: tt)* }) => (
        __symbiosis_build_pattern_types_internal!(input $name {} {} [] $($pattern)*);

    )
}

#[macro_export]
#[doc(hidden)]
macro_rules! __symbiosis_build_pattern_types_internal {
    ($input: ident $name: ident {$($field: ident : $ty: ty),*} {$($read_field: ident: $read_expr: expr;)*} [ $($definitions: tt)* ] {$t: expr} $($rest: tt)*) => (
        __symbiosis_build_pattern_types_internal!(
            $input $name {$($field: $ty),*}
            { $($read_field: $read_expr;)* _t: $input.next(); }
            [ $($definitions)* ]
            $($rest)*
        );
    );
    ($input: ident $name: ident {$($field: ident : $ty: ty),*} {$($read_field: ident: $read_expr: expr;)*} [ $($definitions: tt)* ] $new_field: ident : ($new_type: ident { $($inner: tt)* })? $($rest: tt)*) => (
        __symbiosis_build_pattern_types_internal!(input $new_type {} {} [] $($inner)*);
        __symbiosis_build_pattern_types_internal!(
            $input $name {$($field: $ty,)* $new_field: Option<$new_type>}
            {
                $($read_field: $read_expr;)*
                $new_field: match $input.next() {
                    Some(arg) => try!(
                        arg.into_optional()
                            .map_err(|e| $crate::fragment::Error::expected_optional(Some(e)))
                            .and_then::<Option<$new_type>, _>(|p| if let Some(p) = p {
                                let r = try!($new_type::decode(p));
                                Ok(Some(r))
                            } else {
                                Ok(None)
                            })
                    ),
                    None => return Err($crate::fragment::Error::expected_optional(None))
                };
            }
            [ $($definitions)* ]
            $($rest)*
        );
    );
    ($input: ident $name: ident {$($field: ident : $ty: ty),*} {$($read_field: ident: $read_expr: expr;)*} [ $($definitions: tt)* ] $new_field: ident : [$new_type: ident { $($inner: tt)* }]($($meta: tt)*) $($rest: tt)*) => (
        __symbiosis_build_pattern_types_internal!(input $new_type {} {} [] $($inner)*);
        __symbiosis_build_pattern_types_internal!(
            $input $name {$($field: $ty,)* $new_field: Vec<$new_type>}
            {
                $($read_field: $read_expr;)* 
                $new_field: match $input.next() {
                    Some(arg) => try!(
                        arg.into_repeat()
                            .map_err(|e| $crate::fragment::Error::expected_repeating(Some(e)))
                            .and_then(|v| {
                                let mut res = Vec::with_capacity(v.len());
                                for p in v {
                                    res.push(try!($new_type::decode(p)))
                                }
                                Ok(res)
                            })
                    ),
                    None => return Err($crate::fragment::Error::expected_repeating(None))
                };
            }
            [ $($definitions)* ]
            $($rest)*
        );
    );
    ($input: ident $name: ident {$($field: ident : $ty: ty),*} {$($read_field: ident: $read_expr: expr;)*} [ $($definitions: tt)* ] $new_field: ident : input $($rest: tt)*) => (
        __symbiosis_build_pattern_types_internal!(
            $input $name {$($field: $ty,)* $new_field: $crate::fragment::InputType}
            {
                $($read_field: $read_expr;)*
                $new_field: match $input.next() {
                    Some(arg) => try!(
                        arg.into_input().map_err(|e| $crate::fragment::Error::expected_input(Some(e)))
                    ),
                    None => return Err($crate::fragment::Error::expected_input(None))
                };
            }
            [ $($definitions)* ]
            $($rest)*
        );
    );
    ($input: ident $name: ident {$($field: ident : $ty: ty),*} {$($read_field: ident: $read_expr: expr;)*} [ $($definitions: tt)* ] $new_field: ident : string $($rest: tt)*) => (
        __symbiosis_build_pattern_types_internal!(
            $input $name {$($field: $ty,)* $new_field: $crate::fragment::InputType}
            {
                $($read_field: $read_expr;)*
                $new_field: match $input.next() {
                    Some(arg) => try!(
                        arg.into_string().map_err(|e| $crate::fragment::Error::expected_string(Some(e)))
                    ),
                    None => return Err($crate::fragment::Error::expected_string(None))
                };
            }
            [ $($definitions)* ]
            $($rest)*
        );
    );
    ($input: ident $name: ident {$($field: ident : $ty: ty),*} {$($read_field: ident: $read_expr: expr;)*} [ $($definitions: tt)* ]) => (
        struct $name {$($field: $ty),*}
        impl $name {
            fn decode(args: Vec<$crate::fragment::pattern::Argument>) -> Result<$name, $crate::fragment::Error> {
                let mut $input = args.into_iter();
                $(let $read_field =  $read_expr;)*
                Ok($name {
                    $($field: $field),*
                })
            }
        }

        $($definitions)*
    );
}

///An input pattern, represented as a sequence of components.
pub struct Pattern(Vec<Component>);

impl Pattern {
    ///Create an empty pattern.
    pub fn new() -> Pattern {
        Pattern(Vec::new())
    }

    ///Add a new component to the pattern.
    pub fn push(&mut self, component: Component) {
        self.0.push(component);
    }

    #[doc(hidden)]
    pub fn parse<F>(&self, src: &[Input], mut get_input: F) -> Result<Vec<Argument>, Error> where F: FnMut(&FragmentKind) -> Result<InputType, Error> {
        let mut src = InputSlicer::new(Cow::Borrowed(src));
        self.parse_using(&mut src, &mut get_input)
    }

    fn parse_using<F>(&self, src: &mut InputSlicer, get_input: &mut F) -> Result<Vec<Argument>, Error> where F: FnMut(&FragmentKind) -> Result<InputType, Error> {
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
