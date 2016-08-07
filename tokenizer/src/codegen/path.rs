use std::ops::Deref;
use std::fmt;

use StrTendril;

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Path(Vec<StrTendril>);

impl Path {
    pub fn extend<I: IntoIterator<Item=StrTendril>>(&mut self, iter: I) {
        self.0.extend(iter);
    }
}

impl Deref for Path {
    type Target = [StrTendril];

    fn deref(&self) -> &[StrTendril] {
        &self.0
    }
}

impl fmt::Display for Path {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut parts = self.iter();

        if let Some(first) = parts.next() {
            try!(first.fmt(f));
        }

        for part in parts {
            try!(write!(f, ".{}", part));
        }

        Ok(())
    }
}

impl IntoIterator for Path {
    type IntoIter = <Vec<StrTendril> as IntoIterator>::IntoIter;
    type Item = StrTendril;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a> IntoIterator for &'a Path {
    type IntoIter = <&'a Vec<StrTendril> as IntoIterator>::IntoIter;
    type Item = &'a StrTendril;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl From<Vec<StrTendril>> for Path {
    fn from(path: Vec<StrTendril>) -> Path {
        Path(path)
    }
}

impl From<StrTendril> for Path {
    fn from(path: StrTendril) -> Path {
        Path(vec![path])
    }
}
