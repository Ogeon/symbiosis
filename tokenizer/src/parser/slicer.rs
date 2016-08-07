use tendril::StrTendril;

pub struct Slicer {
    src: StrTendril,
    offset: u32,
    length: u32
}

impl Slicer {
    pub fn new(src: StrTendril) -> Slicer {
        Slicer {
            src: src,
            offset: 0,
            length: 0
        }
    }

    pub fn slice(&mut self) -> StrTendril {
        let slice = self.src.subtendril(self.offset, self.length);
        self.offset += self.length;
        self.length = 0;
        slice
    }

    pub fn slice_excluding(&mut self, nbytes: u32) -> StrTendril {
        let len = if self.length >= nbytes {
            self.length - nbytes
        } else {
            0
        };

        let slice = self.src.subtendril(self.offset, len);
        self.offset += self.length;
        self.length = 0;
        slice
    }

    pub fn discard(&mut self) {
        self.offset += self.length;
        self.length = 0;
    }

    pub fn remaining(&mut self) -> StrTendril {
        self.src.subtendril(self.offset, self.src.len32() - self.offset)
    }

    pub fn next(&mut self) -> Option<u8> {
        let index = self.length + self.offset;
        if index < self.src.len32() {
            self.length += 1;
            Some(self.src.as_bytes()[index as usize])
        } else {
            None
        }
    }

    pub fn next_char(&mut self) -> Option<char> {
        let i = (self.offset + self.length) as usize;
        if let Some(c) = self.src[i..].chars().next() {
            self.length += c.len_utf8() as u32;
            Some(c)
        } else {
            None
        }
    }

    pub fn go_back(&mut self) {
        if self.length > 0 {
            self.length -= 1;
        }
    }

    pub fn slice_len(&self) -> u32 {
        self.length
    }

    pub fn skip_whitespace(&mut self) {
        while let Some(c) = self.next() {
            if !(c as char).is_whitespace() {
                self.go_back();
                self.discard();
                break;
            }
        }
    }

    pub fn take_while<F: FnMut(u8) -> bool>(&mut self, mut pred: F) -> Option<StrTendril> {
        while let Some(c) = self.next() {
            if !pred(c) {
                self.go_back();
                return Some(self.slice());
            }
        }

        None
    }
}
