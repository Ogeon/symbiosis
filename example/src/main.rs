extern crate symbiosis_rust;
#[macro_use] extern crate rustful;
extern crate rand;
extern crate rustc_serialize;

use std::str::{FromStr, from_utf8};
use std::io::Read;
use std::fs::File;
use std::path::Path;
use std::collections::BTreeMap;
use std::sync::{Arc, RwLock};
use std::cmp::max;

use symbiosis_rust::{Template, Collection};

use rustful::{Server, TreeRouter, Context, Response, Handler};
use rustful::Method::Get;
use rustful::StatusCode::{NotFound, InternalServerError};
use rustful::header::ContentType;

use rand::Rng;

use rustc_serialize::json;

mod templates;
mod names;

fn main() {
    //Our "database" of people
    let people = Arc::new(RwLock::new(vec![]));

    let router = insert_routes! {
        TreeRouter::new() => {
            "/" => Get: HandlerFn(people.clone(), display_page),
            ":id" => Get: HandlerFn(people.clone(), display_page),
            "res/:resource" => Get: HandlerFn(people.clone(), get_resource),
            "api" => {
                "more/:from" => Get: HandlerFn(people.clone(), load_more),
                "person/:id" => Get: HandlerFn(people.clone(), load_person)
            }
        }
    };

    if let Err(e) = Server::new().handlers(router).port(8080).run() {
        println!("Could not start the server: {}", e);
    }
}

struct HandlerFn(Arc<RwLock<Vec<Person>>>, for<'a> fn(&'a RwLock<Vec<Person>>, Context, Response));

impl Handler for HandlerFn {
    fn handle_request(&self, context: Context, response: Response) {
        self.1(&*self.0, context, response);
    }
}

///Do the initial page rendering and send it to the client.
fn display_page(people: &RwLock<Vec<Person>>, context: Context, mut response: Response) {
    response.set_header(ContentType(content_type!("text", "html", ("charset", "utf-8"))));

    //Get the id of the requested person, if any, and make sure there are enough generated people
    let len = people.read().unwrap().len();
    let id: Option<usize> = context.variables.get("id").and_then(|v| FromStr::from_str(v).ok());

    let min_len = if let Some(id) = id {
        max(id + 1, 10)
    } else {
        10
    };
    
    if len < min_len {
        gen_people(&mut people.write().unwrap(), min_len);
    }

    //Use a `Vec<u8>` as a byte buffer for the person cards.
    let mut cards = vec![];
    for person in people.read().unwrap().iter().take(10) {
        let str_id = person.id.to_string();
        let card = templates::Card {
            id: &str_id,
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
            let mut writer = response.into_writer();
            match id {
                Some(id) => {
                    //Display info about someone

                    let person = &people.read().unwrap()[id];
                    let more_info = templates::MoreInfo {
                        name: &person.name,
                        age: &person.age.to_string(),
                        supervisor: person.supervisor.as_ref().map(|s| &**s),
                        projects: &Collection::Map(&person.projects)
                    };
                    let document = templates::Document {
                        cards: cards,
                        more_info: Some(&more_info)
                    };
                    document.render_to(&mut writer).ok();
                },
                None => {
                    //No additional info was requested

                    let document = templates::Document {
                        cards: cards,
                        more_info: None
                    };
                    document.render_to(&mut writer).ok();
                }
            }
        },
        Err(e) => {
            //Something went horribly wrong!
            context.log.error(&format!("Card templates generated invalid string: {}", e));
            response.set_status(InternalServerError);
        }
    }
}

///Just some resource loading code.
fn get_resource(_: &RwLock<Vec<Person>>, context: Context, mut response: Response) {
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
fn load_more(people: &RwLock<Vec<Person>>, context: Context, mut response: Response) {
    response.set_header(ContentType(content_type!("application", "json", ("charset", "utf-8"))));

    //Get the id from where the sequence of people starts and make sure we have generated enough
    let len = people.read().unwrap().len();
    let from = context.variables.get("from").and_then(|v| FromStr::from_str(v).ok()).unwrap_or(len);
    
    if len < from + 10 {
        gen_people(&mut people.write().unwrap(), from + 10);
    }

    //Read the people and make a vector of references to them. json::encode likes only sized types
    let people = people.read().unwrap();
    let people_refs: Vec<_> = people[from..from+10].iter().collect();
    
    match json::encode(&people_refs) {
        Ok(json) => {response.into_writer().send(json).ok();},
        Err(e) => {
            context.log.error(&format!("failed to encode people list as json: {}", e));
            response.set_status(InternalServerError);
        }
    }
}

///Generate a bunch of people and send them as JSON.
fn load_person(people: &RwLock<Vec<Person>>, context: Context, mut response: Response) {
    response.set_header(ContentType(content_type!("application", "json", ("charset", "utf-8"))));

    let len = people.read().unwrap().len();
    let id = context.variables.get("id").and_then(|v| FromStr::from_str(v).ok()).unwrap_or(0);
    
    if len <= id {
        gen_people(&mut people.write().unwrap(), id + 1);
    }
    
    match json::encode(&people.read().unwrap()[id]) {
        Ok(json) => {response.into_writer().send(json).ok();},
        Err(e) => {
            context.log.error(&format!("failed to encode people list as json: {}", e));
            response.set_status(InternalServerError);
        }
    }
}



#[derive(RustcEncodable)]
struct Person {
    id: usize,
    name: String,
    age: u8,
    supervisor: Option<String>,
    projects: BTreeMap<String, &'static str>
}

///Generate a bunch of people up to (not including) person `n`.
fn gen_people(people: &mut Vec<Person>, n: usize)  {
    let mut rng = rand::thread_rng();
    let len = people.len();
    let positions = ["manager", "researcher", "developer"];
    people.extend((len..n).map(|id| {
        let age = rng.gen_range(18, 64);
        Person {
            id: id,
            name: names::gen_person(&mut rng),
            age: age,
            supervisor: if age < 25 {
                if 0.7f32 > rng.gen() {
                    Some(names::gen_person(&mut rng))
                } else {
                    None
                }
            } else if age < 35 {
                if 0.3f32 > rng.gen() {
                    Some(names::gen_person(&mut rng))
                } else {
                    None
                }
            } else {
                if 0.1f32 > rng.gen() {
                    Some(names::gen_person(&mut rng))
                } else {
                    None
                }
            },
            projects: (17..rng.gen_range(18, age + 1)).map(|_| (names::gen_project(&mut rng), *rng.choose(&positions).unwrap())).collect()
        }
    }));
}