use rand::Rng;

static FNAME: &'static [&'static str] = &[
    "Carl",
    "Fiona",
    "Martin",
    "Crystal",
    "Joseph",
    "Victoria",
    "Rust",
    "Jessica",
    "Fitzroy",
    "Petronella",
    "Lloyd",
    "Irene",
    "Bobby",
    "Johanna"
];

static LNAME: &'static [&'static str] = &[
    "Wilson",
    "Sykes",
    "Tables",
    "Blank",
    "Adler",
    "Josephson",
    "Carlson",
    "Rabbit",
    "Doe"
];

pub fn gen<R: Rng>(rng: &mut R) -> String {
    format!("{} {}", rng.choose(FNAME).unwrap(), rng.choose(LNAME).unwrap())
}