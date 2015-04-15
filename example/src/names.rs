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

static PREFIX: &'static [&'static str] = &[
    "Super",
    "Mega",
    "Hyper",
    "Ultra",
    "Ascending",
    "Black",
    "White",
    "Red",
    "Blue",
    "Tranquil",
    "Leaping",
    "Octagonal",
    "Running"
];

static SUFFIX: &'static [&'static str] = &[
    "Collider",
    "Net",
    "Dawn",
    "Heron",
    "Crayon",
    "Hurricane",
    "Serpent",
    "Scorpion",
    "Roadrunner",
    "Sapphire",
    "Mountain",
    "Spring",
    "Summer",
    "Autumn",
    "Fall",
    "Winter",
    "Lantern",
    "Tuna",
    "Oyster",
    "Rapid",
    "Storm"
];

pub fn gen_person<R: Rng>(rng: &mut R) -> String {
    format!("{} {}", rng.choose(FNAME).unwrap(), rng.choose(LNAME).unwrap())
}

pub fn gen_project<R: Rng>(rng: &mut R) -> String {
    format!("{} {}", rng.choose(PREFIX).unwrap(), rng.choose(SUFFIX).unwrap())
}