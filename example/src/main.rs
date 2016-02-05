extern crate symbiosis_rust;
#[macro_use] extern crate rustful;
extern crate rand;
extern crate rustc_serialize;

use std::str::FromStr;
use std::io::Read;
use std::path::Path;
use std::collections::BTreeMap;
use std::sync::{Arc, RwLock};
use std::cmp::max;
use std::error::Error;

use symbiosis_rust::Generator;

use rustful::{Server, TreeRouter, Context, Response, Handler};
use rustful::StatusCode;
use rustful::header::ContentType;
use rustful::file::check_path;

use rand::Rng;

use rustc_serialize::json;

mod templates;
mod names;
#[macro_use] mod macros;

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

    let res = Server {
        handlers: router,
        host: 8080.into(),
        content_type: content_type!(Text/Html; Charset = Utf8),
        ..Server::default()
    }.run();

    if let Err(e) = res {
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
fn display_page(people: &RwLock<Vec<Person>>, context: Context, response: Response) {
    //Get the id of the requested person, if any, and make sure there are enough generated people
    let len = people.read().unwrap().len();
    let id: Option<usize> = context.variables.get("id").and_then(|v| FromStr::from_str(&v).ok());

    let min_len = if let Some(id) = id {
        max(id + 1, 10)
    } else {
        10
    };
    
    if len < min_len {
        gen_people(&mut people.write().unwrap(), min_len);
    }

    let people = people.read().unwrap();
    let cards = Generator::new(&people[..10], |person| {
        templates::Card {
            id: person.id.into(),
            name: (&person.name).into(),
            age: person.age.into(),
            supervisor: person.supervisor.as_ref().map(Into::into)
        }
    });

    let result = match id {
        Some(id) => {
            //Display info about someone
            let person = &people[id];
            let more_info = templates::MoreInfo {
                name: (&person.name).into(),
                age: person.age.into(),
                supervisor: person.supervisor.as_ref().map(Into::into),
                projects: (&person.projects).into()
            };
            let document = templates::Document {
                cards: (&cards).into(),
                more_info: Some((&more_info).into())
            };
            document.to_string()
        },
        None => {
            //No additional info was requested
            let document = templates::Document {
                cards: (&cards).into(),
                more_info: None
            };
            document.to_string()
        }
    };

    response.send(result);
}

///Just some resource loading code.
fn get_resource(_: &RwLock<Vec<Person>>, context: Context, mut response: Response) {
    let path = handler_expect!(response, context.variables.get("resource"), StatusCode::Forbidden);
    handler_try!(response, check_path(&*path), StatusCode::Forbidden);
    let full_path = Path::new("res").join(&*path);
    let res = response.send_file(full_path)
        .or_else(|e| e.send_not_found("Found nothing..."))
        .or_else(|e| e.ignore_send_error());

    if let Err((_e, mut response)) = res {
        response.set_status(StatusCode::InternalServerError);
    }
}

///Generate a bunch of people and send them as JSON.
fn load_more(people: &RwLock<Vec<Person>>, context: Context, mut response: Response) {
    response.headers_mut().set(ContentType(content_type!(Application/Json; Charset = Utf8)));

    //Get the id from where the sequence of people starts and make sure we have generated enough
    let len = people.read().unwrap().len();
    let from = context.variables.get("from").and_then(|v| FromStr::from_str(&v).ok()).unwrap_or(len);
    
    if len < from + 10 {
        gen_people(&mut people.write().unwrap(), from + 10);
    }

    //Read the people and make a vector of references to them. json::encode likes only sized types
    let people = people.read().unwrap();
    let people_refs: Vec<_> = people[from..from+10].iter().collect();
    
    match json::encode(&people_refs) {
        Ok(json) => response.send(json),
        Err(e) => {
            println!("failed to encode people list as json: {}", e);
            response.set_status(StatusCode::InternalServerError);
        }
    }
}

///Generate a bunch of people and send them as JSON.
fn load_person(people: &RwLock<Vec<Person>>, context: Context, mut response: Response) {
    response.headers_mut().set(ContentType(content_type!(Application/Json; Charset = Utf8)));

    let len = people.read().unwrap().len();
    let id = context.variables.get("id").and_then(|v| FromStr::from_str(&v).ok()).unwrap_or(0);
    
    if len <= id {
        gen_people(&mut people.write().unwrap(), id + 1);
    }
    
    match json::encode(&people.read().unwrap()[id]) {
        Ok(json) => response.send(json),
        Err(e) => {
            println!("failed to encode people list as json: {}", e);
            response.set_status(StatusCode::InternalServerError);
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