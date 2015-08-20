use std::borrow::Cow;

use tendril::StrTendril;

use codegen::{Scope, Logic, ContentType};

macro_rules! impl_fragment {
    ($(#[$attr:meta])* frag $str_name:expr => $name:ident: |$args:ident| $process:block) => {
        $(#[$attr])*
        pub struct $name;

        impl $crate::fragments::Fragment for $name {
            fn identifier(&self) -> &'static str {
                $str_name
            }

            fn process(&self, $args: Vec<$crate::fragments::InputType>)
            -> Result<$crate::fragments::ReturnType, ::std::borrow::Cow<'static, str>>
            $process
        }
    }
}

pub trait Fragment {
    fn identifier(&self) -> &'static str;
    fn process(&self, args: Vec<InputType>) -> Result<ReturnType, Cow<'static, str>>;
}

impl<F: Fn(Vec<InputType>) -> Result<ReturnType, Cow<'static, str>>> Fragment for (&'static str, F) {
    fn identifier(&self) -> &'static str {
        self.0
    }

    fn process(&self, args: Vec<InputType>) -> Result<ReturnType, Cow<'static, str>> {
        self.1(args)
    }
}

impl<'a, F: Fragment> Fragment for &'a F {
    fn identifier(&self) -> &'static str {
        (*self).identifier()
    }

    fn process(&self, args: Vec<InputType>) -> Result<ReturnType, Cow<'static, str>> {
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
    frag "if" => If: |args| {
        if args.len() == 0 {
            return Err(Cow::Borrowed("if requires at least one argument"));
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
    frag "and" => And: |args| {
        if args.len() == 0 {
            return Err(Cow::Borrowed("and requires at least one argument"));
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
    frag "or" => Or: |args| {
        if args.len() == 0 {
            return Err(Cow::Borrowed("or requires at least one argument"));
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
    frag "not" => Not: |args| {
        if args.len() != 1 {
            return Err(Cow::Borrowed("not will only accept one argument"));
        }

        let cond = match args.into_iter().next().unwrap() {
            InputType::Placeholder(name, _) => Logic::Value(name),
            InputType::Logic(cond) => cond
        };

        Ok(ReturnType::Logic(Logic::Not(Box::new(cond))))
    }
}

impl_fragment!{
    #[doc = "`template(a)` tells the parser that the placeholder `a` is an other template."]
    frag "template" => Template: |args| {
        if args.len() != 1 {
            return Err(Cow::Borrowed("template will only accept one argument"));
        }

        match args.into_iter().next().unwrap() {
            InputType::Placeholder(name, _) => Ok(ReturnType::Placeholder(name, ContentType::Template(false))),
            InputType::Logic(_) => Err(Cow::Borrowed("template expected a placeholder, but found a logic expression"))
        }
    }
}

impl_fragment!{
    #[doc = "Start of a `foreach` scope."]
    #[doc = ""]
    #[doc = "`foreach(element, collection)` or `foreach(key, element, collection)` will repeat"]
    #[doc = "everything within the scope for each `element` (and `key`) in `collection`."]
    frag "foreach" => ForEach: |args| {
        let mut args = args.into_iter();
        let args = (args.next(), args.next(), args.next());
        
        match args {
            (Some(element), Some(collection), None) => {
                let element = match element {
                    InputType::Placeholder(name, _) => name,
                    InputType::Logic(_) => return Err(Cow::Borrowed("foreach expected a placeholder as `element`, but found a logic expression"))
                };

                let collection = match collection {
                    InputType::Placeholder(name, _) => name,
                    InputType::Logic(_) => return Err(Cow::Borrowed("foreach expected a placeholder as `collection`, but found a logic expression"))
                };

                Ok(ReturnType::Scope(Scope::ForEach(collection, element, None)))
            },
            (Some(key), Some(element), Some(collection)) => {
                let key = match key {
                    InputType::Placeholder(name, _) => name,
                    InputType::Logic(_) => return Err(Cow::Borrowed("foreach expected a placeholder as `key`, but found a logic expression"))
                };

                let element = match element {
                    InputType::Placeholder(name, _) => name,
                    InputType::Logic(_) => return Err(Cow::Borrowed("foreach expected a placeholder as `element`, but found a logic expression"))
                };

                let collection = match collection {
                    InputType::Placeholder(name, _) => name,
                    InputType::Logic(_) => return Err(Cow::Borrowed("foreach expected a placeholder as `collection`, but found a logic expression"))
                };

                Ok(ReturnType::Scope(Scope::ForEach(collection, element, Some(key))))
            },
            _ => Err(Cow::Borrowed("foreach will only accept two or three argumens"))
        }
    }
}