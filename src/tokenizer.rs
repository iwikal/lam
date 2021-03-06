use crate::interner::{InternedHandle, Interner};
use std::cell::RefCell;
use std::fmt;
use std::io;
use std::iter::Peekable;
use std::rc::Rc;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Token {
    Ident(InternedHandle),
    Value(Value),
    Char(char),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Value {
    U8(u128),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::U8(val) => write!(f, "{}", val),
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Ident(_) => write!(f, "<identifier>"),
            Self::Value(val) => val.fmt(f),
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
            &Token::Value(val) => val.fmt(f),
            &Token::Char(ch) => write!(f, "'{}'", ch),
        }
    }
}

pub struct Tokenizer<I: Iterator> {
    chars: Peekable<I>,
    interner: Rc<RefCell<Interner>>,
}

impl<I: Iterator> Tokenizer<I> {
    pub fn new(chars: I, interner: Interner) -> Self {
        Self {
            chars: chars.peekable(),
            interner: Rc::new(RefCell::new(interner)),
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
    pub fn get_interner(&self) -> &Rc<RefCell<Interner>> {
        &self.interner
    }

    fn get_ident(&mut self) -> io::Result<InternedHandle> {
        let mut word = String::new();

        let mut interner = self.interner.borrow_mut();

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

    fn get_number(&mut self) -> io::Result<Value> {
        let mut radix = 10;
        let mut acc = 0;
        let mut count = 0;

        Ok(Value::U8(loop {
            match self.chars.peek() {
                Some(&Ok('0')) if count == 0 => {
                    self.chars.next();
                    match self.chars.peek() {
                        Some(Ok('x')) => {
                            self.chars.next();
                            radix = 0x10;
                        }
                        Some(&Ok(ch)) if !ch.is_ascii_digit() => break 0,
                        _ => panic!("wrong"),
                    }
                }
                Some(&Ok(ch)) if ch.is_digit(radix) => {
                    let n = ch.to_digit(radix).unwrap();
                    acc = u128::checked_mul(acc, radix as _)
                        .unwrap_or_else(|| panic!("too big numeric constant"));
                    acc += n as u128;
                    self.chars.next();
                }
                Some(&Ok(_)) | None => {
                    break acc;
                }
                Some(Err(_)) => {
                    return self.chars.next().unwrap().map(|_| unreachable!());
                }
            }
            count += 1;
        }))
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
                Ok('0'..='9') => break self.get_number().map(Token::Value),
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
