extern crate symbiosis_rust;
#[macro_use] extern crate rustful;
extern crate rand;
extern crate rustc_serialize;

use std::str::from_utf8;
use std::io::Read;
use std::fs::File;
use std::path::Path;

use symbiosis_rust::Template;

use rustful::{Server, TreeRouter, Context, Response, Handler};
use rustful::Method::Get;
use rustful::StatusCode::{NotFound, InternalServerError};
use rustful::header::ContentType;

use rand::Rng;

use rustc_serialize::json;

mod templates;
mod names;

fn main() {
    let router = insert_routes! {
        TreeRouter::new() => {
            "/" => Get: HandlerFn(display_page),
            "/res/:resource" => Get: HandlerFn(get_resource),
            "/api/more" => Get: HandlerFn(load_more)
        }
    };

    if let Err(e) = Server::new().handlers(router).port(8080).run() {
        println!("Could not start the server: {}", e);
    }
}

struct HandlerFn(fn(Context, Response));

impl Handler for HandlerFn {
    fn handle_request(&self, context: Context, response: Response) {
        self.0(context, response);
    }
}

///Do the initial page rendering and send it to the client.
fn display_page(context: Context, mut response: Response) {
    response.set_header(ContentType(content_type!("text", "html", ("charset", "utf-8"))));

    //Use a `Vec<u8>` as a byte buffer for the person cards.
    let mut cards = vec![];
    for person in gen_people() {
        let card = templates::Card {
            name: &person.name,
            age: &person.age.to_string(),
            supervisor: person.supervisor.as_ref().map(|s| &**s)
        };
        card.render_to(&mut cards).ok();
    }

    //Use the buffer as a &str in the document template.
    //This should always work as long as the templates are correct.
    match from_utf8(&cards) {
        Ok(cards) => {
            let document = templates::Document {
                cards: cards
            };
            let mut writer = response.into_writer();
            document.render_to(&mut writer).ok();
        },
        Err(e) => {
            //Something went horribly wrong!
            context.log.error(&format!("Card templates generated invalid string: {}", e));
            response.set_status(InternalServerError);
        }
    }
}

///Just some resource loading code.
fn get_resource(context: Context, mut response: Response) {
    let base = Path::new("res");

    let filename = match context.variables.get("resource") {
        Some(f) => f,
        None => {
            response.set_status(NotFound);
            return;
        }
    };
    let path = base.join(filename);
    let (primary, secondary) = match path.extension().and_then(|e| e.to_str()) {
        Some("css") => ("text", "css"),
        Some("js") => ("text", "javascript"),
        _ => {
            response.set_status(NotFound);
            return;
        }
    };
    response.set_header(ContentType(content_type!(primary, secondary)));

    let mut buf = vec![0;512];

    let mut file = match File::open(&path) {
        Ok(f) => f,
        Err(_) => {
            response.set_status(NotFound);
            return;
        }
    };

    let mut writer = response.into_writer();

    loop {
        match file.read(&mut buf) {
            Ok(n) if n == 0 => break,
            Err(_) => break,
            Ok(n) => {writer.send(&buf[..n]).ok();}
        }
    }
}

///Generate a bunch of people and send them as JSON.
fn load_more(context: Context, mut response: Response) {
    response.set_header(ContentType(content_type!("application", "json", ("charset", "utf-8"))));
    match json::encode(&gen_people()) {
        Ok(json) => {response.into_writer().send(json).ok();},
        Err(e) => {
            context.log.error(&format!("failed to encode people list as json: {}", e));
            response.set_status(InternalServerError);
        }
    }
}

#[derive(RustcEncodable)]
struct Person {
    name: String,
    age: u8,
    supervisor: Option<String>
}

///Generate a bunch of people.
fn gen_people() -> Vec<Person> {
    let mut rng = rand::thread_rng();
    (0..10).map(|_| {
        let age = rng.gen_range(18, 64);
        Person {
            name: names::gen(&mut rng),
            age: age,
            supervisor: if age < 25 {
                if 0.7f32 > rng.gen() {
                    Some(names::gen(&mut rng))
                } else {
                    None
                }
            } else if age < 35 {
                if 0.3f32 > rng.gen() {
                    Some(names::gen(&mut rng))
                } else {
                    None
                }
            } else {
                if 0.1f32 > rng.gen() {
                    Some(names::gen(&mut rng))
                } else {
                    None
                }
            }
        }
    }).collect()
}