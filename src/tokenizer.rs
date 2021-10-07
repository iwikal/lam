use crate::interner::{InternedHandle, Interner};
use std::rc::Rc;
use std::sync::RwLock;
use std::fmt;
use std::io;
use std::iter::Peekable;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Token {
    Ident(InternedHandle),
    Number(u64),
    Char(char),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Ident(_) => write!(f, "<identifier>"),
            Self::Number(num) => num.fmt(f),
            Self::Char(ch) => write!(f, "'{}'", ch),
        }
    }
}

pub struct TokenFmt<'int>(pub Token, pub &'int Interner);

impl<'int> fmt::Display for TokenFmt<'int> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Self(tok, int) = self;

        match tok {
            &Token::Ident(id) => write!(f, "{}", int.lookup(id)),
            &Token::Number(num) => num.fmt(f),
            &Token::Char(ch) => write!(f, "'{}'", ch),
        }
    }
}

pub struct Tokenizer<I: Iterator> {
    chars: Peekable<I>,
    interner: Rc<RwLock<Interner>>,
}

impl<I: Iterator> Tokenizer<I> {
    pub fn new(chars: I, interner: Interner) -> Self {
        Self {
            chars: chars.peekable(),
            interner: Rc::new(RwLock::new(interner)),
        }
    }
}

fn is_ident(ch: char) -> bool {
    ch == '_' || ch.is_alphanumeric()
}

fn is_ident_initial(ch: char) -> bool {
    ch == '_' || ch.is_alphabetic()
}

impl<I> Tokenizer<I>
where
    I: Iterator<Item = io::Result<char>>,
{
    pub fn get_interner(&self) -> &Rc<RwLock<Interner>> {
        &self.interner
    }

    fn get_ident(&mut self) -> io::Result<InternedHandle> {
        let mut word = String::new();

        let mut interner = self.interner.write().unwrap();

        loop {
            match self.chars.peek() {
                Some(&Ok(ch)) if is_ident(ch) => {
                    word.push(ch);
                    self.chars.next();
                }
                Some(&Ok(_)) | None => {
                    let interned = interner.intern(&word);
                    break Ok(interned);
                }
                Some(&Err(_)) => {
                    break self.chars.next().unwrap().map(|_| unreachable!());
                }
            }
        }
    }

    fn get_number(&mut self) -> io::Result<u64> {
        let mut acc = 0;

        loop {
            match self.chars.peek() {
                Some(&Ok(ch)) if ch.is_ascii_digit() => {
                    let n = ch.to_digit(10).unwrap();
                    acc *= 10;
                    acc += n as u64;
                    self.chars.next();
                }
                Some(&Ok(_)) | None => {
                    break Ok(acc);
                }
                Some(&Err(_)) => {
                    break self.chars.next().unwrap().map(|_| unreachable!());
                }
            }
        }
    }
}

impl<I> Iterator for Tokenizer<I>
where
    I: Iterator<Item = io::Result<char>>,
{
    type Item = io::Result<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        Some(loop {
            match *self.chars.peek()? {
                Ok(ch) if ch.is_whitespace() => {
                    self.chars.next();
                }
                Ok(ch) if is_ident_initial(ch) => break self.get_ident().map(Token::Ident),
                Ok('0'..='9') => break self.get_number().map(Token::Number),
                Ok(ch) => {
                    self.chars.next();
                    break Ok(Token::Char(ch));
                }
                Err(_) => {
                    let err = self.chars.next().unwrap().unwrap_err();
                    break Err(err);
                }
            }
        })
    }
}
