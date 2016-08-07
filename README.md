# Symbiosis

[![Build Status](https://travis-ci.org/Ogeon/symbiosis.svg?branch=master)](https://travis-ci.org/Ogeon/symbiosis)

Parser and code generation library for HTML templates, allowing template logic
to be shared between Rust, JavaScript and other languages. HTML code is parsed
and transformed into a generic language, which is then fed into code
generators. This makes it possible to generate native templates independent of
external files and statically typed languages can still benefit from its type
checking.

See the `example` directory for a simple example of how it can be used.

## Template Syntax

The syntax is simple and similar to what other template libraries, like
Handlebars and Mustache, has. The file is written as regular HTML with special
expressions, called fragments, where you want something special to happen. The
most basic fragment is the placeholder:

```html
<title>{{ page_title }}</title>
```

It's a simple identifier or sometimes a path (`{{ path.to.content }}`) that points
to the content.

There are also more complex fragments, like `if`, `foreach`, etc., that can be
used to make a more dynamic template. These are backed up by Rust functions
and it's possible to write your own fragments. The `foreach` fragment can look
like this when in use:

```html
<ul>
{{ foreach(title => description in books) }}
	<li>{{ title }}: {{ description }}</li>
{{ end }}
</ul>
```

Notice how it looks like a function call. The basic anatomy of such a fragment
is `name(arguments)`, where the `arguments` part consists of more fragments,
strings or "other" tokens (like "=>" or "in" in the example above).

Notice also the `end` fragment. It's the only keyword in Symbiosis, and will
always be interpreted as the end of a scope. It can therefore not be used as a
placeholder.

## Types

The short introduction above mentioned static typing, but none of the above
fragment examples does really show much sign of it. Well, that's the beauty of
it all. Types are inferred based on how they are used and will end up as one
of a few basic types:

 * Content, if used as placeholders or as key in `foreach`. Can be textual, numeric, or anything that can reasonably be represented as a string.
 * Logic, if used in `if`. Basically Boolean values.
 * Collection, if used after `in` in `foreach`. Can be lists or maps.
 * Data structure, if any of its fields are accessed in a path.

Values can actually be both logic and something else. This will result in an
optional value, which is represented as `Option<T>` in Rust or as `null`/`T`
where that's the convention. The existence of a value is considered `true` and
its absence is considered `false`. Empty collections and strings are also
`true`.

The only really tricky part with this is the data structures. Fragments (like
the `struct_name` fragment) may name them, but there may still be unnamed
ones. The solution for this problem is to generate a name from the access path
to where it's used, so the type of `meta` in `{{ meta.date }}`, in the
template `Entry`, could get the name `EntryMeta`. Some languages, like
JavaScript, doesn't really care about named structures, so that problem
doesn't exist there.

## The Crates

### [`symbiosis`](http://ogeon.github.io/docs/symbiosis/master/symbiosis)

This is where the code generators resides. Include it in your build script
like this:

```rust
//in build.rs
extern crate symbiosis;

use std::fs::File;
use std::path::Path;

use symbiosis::TemplateGroup;
use symbiosis::rust::Rust;

fn main() {
    let mut templates = TemplateGroup::new();
    //Parse the .html files in the directory "templates" (non-recursively)
    templates.parse_directory("templates").expect("failed to parse templates");

    let codegen = Rust::default();

    let out_dir = ::std::env::var("OUT_DIR").unwrap();
    let dest_path = Path::new(&out_dir).join("templates.rs");
    let mut file = File::create(dest_path).expect("couldn't create templates.rs");
    templates.emit_code(&mut file, &codegen).expect("failed to emit Rust code");
}
```

and include the emitted code in your project like this:

```rust
//in src/templates.rs (not the same as we wrote to!)
include!(concat!(env!("OUT_DIR"), "/templates.rs"));
```

You will also need to add `symbiosis_static` to your project. See the next
crate for that.

### [`symbiosis_static`](http://ogeon.github.io/docs/symbiosis/master/symbiosis_static)

This is a utility crate, which provides the necessary types for the static
templates. Include it in your crate root like this:

```rust
//in main.rs or lib.rs
extern crate symbiosis_static; //This is often enough
```

### [`symbiosis_dynamic`](http://ogeon.github.io/docs/symbiosis/master/symbiosis_dynamic)

It's sometimes not practical or even possible to use static templates, which
is why `symbiosis_dynamic` exists. It uses the same system as the static
templates, but the token stream is instead reinterpreted into a more compact
format and kept in memory. Include it in your project like this:

```rust
//in main.rs or lib.rs
extern crate symbiosis_dynamic;

fn template_stuff(src: String) {
	//The Parser is similar to TemplateGroup above
    let mut parser = Parser::new();
	let template = parser.parse_string(src).expect("failed to parse template");
	//...
}
```

### [`symbiosis_tokenizer`](http://ogeon.github.io/docs/symbiosis/master/symbiosis_tokenizer)

This is the heart of the whole system. The tokenizer interprets the HTML
generates the token stream, and it's what powers both the static and the
dynamic templates.
