use std::collections::HashMap;
use std::borrow::Borrow;
use std::hash::Hash;
use std::fmt;

use StrTendril;

use codegen::{Scope, Logic, ContentType, Path, Name, Params};
pub use parser::{Parser, Token, Error, ErrorKind};

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

pub trait Fragment {
    fn identifier(&self) -> &'static str;
    fn process(&self, args: &mut Parser) -> Result<ReturnType, Error>;
}

impl<F: Fn(&mut Parser) -> Result<ReturnType, Error>> Fragment for (&'static str, F) {
    fn identifier(&self) -> &'static str {
        self.0
    }

    fn process(&self, parser: &mut Parser) -> Result<ReturnType, Error> {
        self.1(parser)
    }
}

impl<'a, F: Fragment> Fragment for &'a F {
    fn identifier(&self) -> &'static str {
        (*self).identifier()
    }

    fn process(&self, parser: &mut Parser) -> Result<ReturnType, Error> {
        (*self).process(parser)
    }
}

///Things that can be returned from fragments.
#[derive(Debug)]
pub enum ReturnType {
    ///A plain text string.
    String(StrTendril),
    ///A placeholder parameter and its preferred content type.
    Placeholder(Path, ContentType),
    ///A logic expression.
    Logic(Logic),
    ///The beginning of a scope.
    Scope(Scope),
    ///The end of a scope.
    End,
    ///An HTML tag tree.
    Tag {
        name: Name,
        arguments: Vec<(Name, Vec<ReturnType>)>,
        content: Option<Vec<ReturnType>>,
    },
    ///Give a placeholder a preferred content type, but don't use it.
    TypeHint(Path, ContentType),
}

pub trait FragmentStore {
    fn get(&self, ident: &str) -> Option<&Fragment>;
    fn insert(&mut self, fragment: Box<Fragment>);
}

impl<S: Hash + Eq + Borrow<str> + From<&'static str>> FragmentStore for HashMap<S, Box<Fragment>> {
    fn get(&self, ident: &str) -> Option<&Fragment> {
        self.get(ident).map(AsRef::as_ref)
    }

    fn insert(&mut self, fragment: Box<Fragment>) {
        self.insert(fragment.identifier().into(), fragment);
    }
}

///Initiate the default fragments.
pub fn init_prelude<S: FragmentStore>(store: &mut S) {
    store.insert(Box::new(If));
    store.insert(Box::new(And));
    store.insert(Box::new(Or));
    store.insert(Box::new(Not));
    store.insert(Box::new(ForEach));
    store.insert(Box::new(StructName));
}

///Start of an `if` scope.
///
///Anything between `{{ if(...) }}` and `{{ end }}` will
///be hidden if the condition is false. `if(a, b, ...)` will be interpreted as
///`if(and(a, b, ...))`.
///
///```text
///{{ if(name) }}<p>My name is {{ name }}</p>{{ end }}
///{{ if(name, age) }}<p>My name is {{ name }} and I am {{ age }} years old.</p>{{ end }}
///{{ if(am_a_teapot) }}<p>I am a teapot</p>{{ end }}
///```
pub struct If;

impl Fragment for If {
    fn identifier(&self) -> &'static str {
        "if"
    }

    fn process(&self, parser: &mut Parser) -> Result<ReturnType, Error> {
        let mut args = vec![try!(parser.input())];
        while parser.token(Token::Other(",".into())).is_ok() {
            args.push(try!(parser.input()));
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

///`and` logic fragment.
///
///`and(a, b, ...)` will only be true if all of the
///conditions `a, b, ...` are true.
pub struct And;

impl Fragment for And {
    fn identifier(&self) -> &'static str {
        "and"
    }

    fn process(&self, parser: &mut Parser) -> Result<ReturnType, Error> {
        let mut args = vec![try!(parser.input())];
        while parser.token(Token::Other(",".into())).is_ok() {
            args.push(try!(parser.input()));
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

///`or` logic fragment.
///
///`or(a, b, ...)` will be true if one of the conditions
///`a, b, ...` is true.
pub struct Or;

impl Fragment for Or {
    fn identifier(&self) -> &'static str {
        "or"
    }

    fn process(&self, parser: &mut Parser) -> Result<ReturnType, Error> {
        let mut args = vec![try!(parser.input())];
        while parser.token(Token::Other(",".into())).is_ok() {
            args.push(try!(parser.input()));
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

///`not` logic fragment.
///
///`not(a)` will be true if condition `a` is false.
pub struct Not;

impl Fragment for Not {
    fn identifier(&self) -> &'static str {
        "not"
    }

    fn process(&self, parser: &mut Parser) -> Result<ReturnType, Error> {
        let cond = match try!(parser.input()) {
            InputType::Placeholder(name, _) => Logic::Value(name),
            InputType::Logic(cond) => cond
        };

        Ok(ReturnType::Logic(Logic::Not(Box::new(cond))))
    }
}

///Start of a `foreach` scope.
///
///`foreach(element in collection)` or `foreach(key => element in collection)` will repeat
///everything within the scope for each `element` (and `key`) in `collection`.
pub struct ForEach;

impl Fragment for ForEach {
    fn identifier(&self) -> &'static str {
        "foreach"
    }

    fn process(&self, parser: &mut Parser) -> Result<ReturnType, Error> {
        let first = try!(parser.identifier());
        let (key, element) = if parser.token(Token::Other("=>".into())).is_ok() {
            (Some(first), try!(parser.identifier()))
        } else {
            (None, first)
        };
        try!(parser.token(Token::Ident("in".into())));
        Ok(ReturnType::Scope(Scope::ForEach(try!(parser.path()), element, key)))
    }
}

///Name a data structure.
///
///`struct_name(some_struct is "StructName")` will tell the code generator
///that the value `some_struct` should be of the type `StructName`.
pub struct StructName;

impl Fragment for StructName {
    fn identifier(&self) -> &'static str {
        "struct_name"
    }

    fn process(&self, parser: &mut Parser) -> Result<ReturnType, Error> {
        let path = try!(parser.path());
        try!(parser.token(Token::Ident("is".into())));
        Ok(ReturnType::TypeHint(path, ContentType::Struct(Some(try!(parser.string()).into()), Params::new(), false)))
    }
}