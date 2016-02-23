use std::cmp::min;

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

    pub fn offset(&self) -> u32 {
        self.offset
    }

    pub fn jump_to(&mut self, offset: u32) {
        self.offset = min(offset, self.src.len32());
        self.length = 0;
    }

    pub fn jump_back_by(&mut self, offset: u32) {
        if offset > self.offset {
            self.offset = 0;
        } else {
            self.offset -= offset;
        }
        self.length = 0;
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

    pub fn eat(&mut self, byte: u8) -> bool {
        match self.next() {
            Some(c) if c == byte => {
                self.discard();
                true
            },
            Some(_) => {
                self.go_back();
                false
            },
            None => false
        }
    }

    pub fn eat_bytes(&mut self, bytes: &[u8]) -> bool {
        let snapshot = self.length;

        for &byte in bytes {
            match self.next() {
                Some(c) => if c != byte {
                    self.length = snapshot;
                    return false;
                },
                None => {
                    self.length = snapshot;
                    return false;
                }
            }
        }

        self.discard();
        
        true
    }
}
