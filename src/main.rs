use std::collections::HashMap;
use std::rc::Rc;
use std::sync::RwLock;
use std::fmt;
use std::fmt::Write;
use std::io;
use std::io::prelude::*;
use utf8_decode::UnsafeDecoder;

mod interner;
mod tokenizer;

use interner::{InternedHandle, Interner};
use tokenizer::{Token, Tokenizer};

#[derive(Clone)]
struct Definition<'a> {
    parameters: Vec<InternedHandle>,
    body: Vec<Token>,
    closure: Scopes<'a>,
    interner: Rc<RwLock<Interner>>,
}

impl<'a> fmt::Display for Definition<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let interner = self.interner.read().unwrap();
        for &param in &self.parameters {
            write!(f, "{} -> ", interner.lookup(param))?;
        }
        for &token in &self.body {
            tokenizer::TokenFmt(token, &*interner).fmt(f)?;
        }
        Ok(())
    }
}

type Scope<'a> = HashMap<InternedHandle, Definition<'a>>;

#[derive(Clone)]
struct Scopes<'a>(Scope<'a>, Option<&'a Scopes<'a>>);

type Stack = Vec<u64>;

struct Stackframe<'a> {
    inner: &'a mut Stack,
    offset: usize,
}

impl<'a> Stackframe<'a> {
    fn from_stack(inner: &'a mut Stack) -> Self {
        Self { inner, offset: 0 }
    }

    fn with_offset(&mut self, offset: usize) -> Stackframe {
        Stackframe {
            inner: self.inner,
            offset: self.offset + offset,
        }
    }

    fn push(&mut self, val: u64) {
        self.inner.push(val)
    }

    fn pop(&mut self) -> Option<u64> {
        self.inner.pop()
        /*
        if self.inner.len() > self.offset {
            self.inner.pop()
        } else {
            None
        }
        */
    }
}

impl<'a> std::ops::Deref for Stackframe<'a> {
    type Target = [u64];

    fn deref(&self) -> &[u64] {
        self.inner
        // &self.inner[self.offset..]
    }
}

impl<'a> std::ops::DerefMut for Stackframe<'a> {
    fn deref_mut(&mut self) -> &mut [u64] {
        self.inner
        // &mut self.inner[self.offset..]
    }
}

struct IdentGuard<'a> {
    lock: std::sync::RwLockReadGuard<'a, Interner>,
    ident: &'a str,
}

impl<'a> std::ops::Deref for IdentGuard<'a> {
    type Target = str;

    fn deref(&self) -> &str {
        self.ident
    }
}

impl<'a> fmt::Display for IdentGuard<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.ident.fmt(f)
    }
}

trait Context {
    type Tokens: Iterator<Item = io::Result<Token>>;
    fn identifiers(&self) -> &Rc<RwLock<Interner>>;
    fn tokens(&mut self) -> &mut Self::Tokens;
}

impl<I: Iterator<Item = io::Result<char>>> Context for Tokenizer<I> {
    type Tokens = Tokenizer<I>;

    fn tokens(&mut self) -> &mut Self::Tokens {
        self
    }

    fn identifiers(&self) -> &Rc<RwLock<Interner>> {
        Tokenizer::get_interner(self)
    }
}

type VecTokenIter<'a> =
    std::iter::Map<std::iter::Cloned<std::slice::Iter<'a, Token>>, fn(Token) -> io::Result<Token>>;

struct CallContext<'v>(VecTokenIter<'v>, Rc<RwLock<Interner>>);

impl<'v> Context for CallContext<'v> {
    type Tokens = VecTokenIter<'v>;

    fn tokens(&mut self) -> &mut Self::Tokens {
        &mut self.0
    }

    fn identifiers(&self) -> &Rc<RwLock<Interner>> {
        &self.1
    }
}

fn call<C>(
    mut context: C,
    mut stack: Stackframe,
    keywords: &Keywords,
    scopes: Option<&Scopes>,
) -> io::Result<()>
where
    C: Context,
{
    let mut scopes = Scopes(HashMap::new(), scopes);

    'top_loop: loop {
        let tok = match context.tokens().next() {
            Some(result) => result?,
            None => break,
        };

        match tok {
            Token::Ident(id) if id == keywords.let_ => {
                let mut signature = vec![];
                for tok in context.tokens() {
                    let tok = tok?;
                    match tok {
                        Token::Ident(id) => signature.push(id),
                        Token::Char('=') => break,
                        _ => panic!("unexpected token {}", tok),
                    }
                }

                let name = match signature.pop() {
                    Some(name) => name,
                    _ => panic!("cannot have unnamed function"),
                };

                let mut body = vec![];

                let mut level = 0;
                for tok in &mut context.tokens() {
                    let tok = tok?;
                    match tok {
                        Token::Char(';') if level == 0 => {
                            let Scopes(scope, outer_scopes) = &mut scopes;
                            scope.insert(
                                name,
                                Definition {
                                    parameters: signature,
                                    body,
                                    closure: Scopes(HashMap::clone(&*scope), *outer_scopes),
                                    interner: Rc::clone(context.identifiers()),
                                },
                            );
                            continue 'top_loop;
                        }
                        Token::Ident(id) if id == keywords.let_ => {
                            level += 1;
                            body.push(tok);
                        }
                        Token::Char(';') => {
                            level -= 1;
                            body.push(tok);
                        }
                        _ => body.push(tok),
                    }
                }

                panic!("unexpected end of file, expected {}", Token::Char(';'));
            }
            Token::Ident(id) if id == keywords.print => {
                println!("{}", stack.pop().expect("`print` expected 1 arguments, 0 supplied"));
            }
            Token::Ident(id) if id == keywords.dup => {
                stack.push(*stack.last().expect("`dup` expected 1 arguments, 0 supplied"));
            }
            Token::Ident(id) if id == keywords.swap => {
                let len = stack.len();
                stack.swap(len - 1, len - 2);
            }
            Token::Ident(id) if id == keywords.drop => {
                match stack.pop() {
                    Some(_) => {}
                    None => {}
                }
                stack.pop().expect("`drop` expected 1 arguments, 0 supplied");
            }
            Token::Ident(id) if id == keywords.over => {
                stack.push(stack[stack.len() - 2]);
            }
            Token::Number(num) => stack.push(num),
            Token::Char('+') => {
                let a = stack.pop().expect("operator '+' expected two arguments, 0 supplied");
                let b = stack.pop().expect("operator '+' expected two arguments, 1 supplied");
                stack.push(a.wrapping_add(b));
            }
            Token::Char(ch) => {
                panic!("unexpected character {}", ch);
            }
            Token::Ident(ident) => {
                let mut scopes = &scopes;
                loop {
                    let format_ident = || {
                        let mut buf = String::new();
                        let interner = context.identifiers().read().unwrap();
                        let _ = write!(buf, "{}", interner.lookup(ident));
                        buf
                    };

                    let Scopes(scope, outer_scopes) = scopes;
                    if let Some(def) = scope.get(&ident) {
                        let Definition {
                            parameters,
                            body,
                            closure,
                            interner,
                        } = def;

                        let mut bindings = HashMap::new();
                        for (i, &param) in parameters.iter().enumerate() {
                            let def = Definition {
                                parameters: vec![],
                                body: vec![Token::Number(stack.pop().unwrap_or_else(|| {
                                    panic!(
                                        "`{}` expected {} arguments, {} supplied",
                                        format_ident(),
                                        parameters.len(),
                                        i,
                                    );
                                }))],
                                closure: Scopes(HashMap::new(), None),
                                interner: Rc::clone(interner),
                            };

                            bindings.insert(param, def);
                        }
                        let tokens = body.iter().cloned().map(Result::Ok as _);
                        call(
                            CallContext(tokens, Rc::clone(context.identifiers())),
                            stack.with_offset(stack.len()),
                            keywords,
                            Some(&Scopes(bindings, Some(closure))),
                        )?;
                        break;
                    } else if let Some(outer) = outer_scopes {
                        scopes = outer;
                    } else {
                        panic!("unknown identifier `{}`", format_ident());
                    }
                }
            }
        }
    }

    Ok(())
}

struct Keywords {
    print: InternedHandle,
    dup: InternedHandle,
    swap: InternedHandle,
    drop: InternedHandle,
    over: InternedHandle,
    let_: InternedHandle,
}

fn main() -> io::Result<()> {
    let mut interner = Interner::new();

    let keywords = Keywords {
        print: interner.intern("print"),
        dup: interner.intern("dup"),
        swap: interner.intern("swap"),
        drop: interner.intern("drop"),
        over: interner.intern("over"),
        let_: interner.intern("let"),
    };

    let mut stack: Stack = vec![];

    let bytes = io::stdin().bytes();
    let chars = UnsafeDecoder::new(bytes);
    let tokens = Tokenizer::new(chars, interner);

    call(tokens, Stackframe::from_stack(&mut stack), &keywords, None)?;

    Ok(())
}
