use std::cell::RefCell;
use std::collections::{hash_map::Entry, HashMap};
use std::fmt;
use std::fmt::Write;
use std::io;
use std::io::prelude::*;
use std::rc::Rc;
use utf8_decode::UnsafeDecoder;

mod interner;
mod tokenizer;

use interner::{InternedHandle, Interner};
use tokenizer::{Token, Tokenizer};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum Parameter {
    Value(u64),
    Argument(InternedHandle),
}

#[derive(Clone)]
struct Implementation {
    parameters: Vec<Parameter>,
    body: Vec<Token>,
    closure: Option<Rc<Scopes>>,
    interner: Rc<RefCell<Interner>>,
}

impl Implementation {
    fn match_parameters(
        &self,
        name: InternedHandle,
        stack: &mut Stackframe,
    ) -> Result<Scope, ()> {
        let format_name = || {
            let mut buf = String::new();
            let interner = self.interner.borrow();
            let _ = write!(buf, "{}", interner.lookup(name));
            buf
        };

        let stack_end_offset = stack
            .len()
            .checked_sub(self.parameters.len())
            .unwrap_or_else(|| {
                panic!(
                    "`{}` expected {} arguments, {} supplied",
                    format_name(),
                    self.parameters.len(),
                    stack.len(),
                );
            });

        let mut taken_args = stack[stack_end_offset..].to_vec();

        let mut bindings = Scope::new();

        for &param in self.parameters.iter().rev() {
            let stack_value = taken_args.pop().unwrap();

            match param {
                Parameter::Argument(arg_name) => {
                    let implementation = Implementation {
                        parameters: vec![],
                        body: vec![Token::Number(stack_value)],
                        closure: None,
                        interner: Rc::clone(&self.interner),
                    };

                    let def = Definition {
                        overloads: vec![implementation],
                    };

                    bindings.insert(arg_name, def);
                }
                Parameter::Value(value) => {
                    if stack_value != value {
                        return Err(());
                    }
                }
            }
        }

        stack.remove(self.parameters.len());

        Ok(bindings)
    }
}

impl fmt::Display for Implementation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let interner = self.interner.borrow();
        for &param in &self.parameters {
            match param {
                Parameter::Value(val) => write!(f, "{} -> ", val),
                Parameter::Argument(ident) => {
                    let name = interner.lookup(ident);
                    write!(f, "{} -> ", name)
                }
            }?;
        }
        for &token in &self.body {
            tokenizer::TokenFmt(token, &*interner).fmt(f)?;
        }
        Ok(())
    }
}

impl fmt::Debug for Implementation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Clone)]
struct Definition {
    overloads: Vec<Implementation>,
}

type Scope = HashMap<InternedHandle, Definition>;

#[derive(Clone)]
struct Scopes(RefCell<Scope>, Option<Rc<Scopes>>);

impl Scopes {
    fn new(parent: Option<Rc<Self>>) -> Self {
        Self::with_defs(RefCell::new(Scope::new()), parent)
    }

    fn with_defs(defs: RefCell<Scope>, parent: Option<Rc<Self>>) -> Self {
        Self(defs, parent)
    }
}

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
        if self.inner.len() > self.offset {
            self.inner.pop()
        } else {
            None
        }
    }

    fn remove(&mut self, count: usize) {
        self.inner.truncate(self.inner.len() - count)
    }
}

impl<'a> std::ops::Deref for Stackframe<'a> {
    type Target = [u64];

    fn deref(&self) -> &[u64] {
        &self.inner[self.offset..]
    }
}

impl<'a> std::ops::DerefMut for Stackframe<'a> {
    fn deref_mut(&mut self) -> &mut [u64] {
        &mut self.inner[self.offset..]
    }
}

trait Context {
    type Tokens: Iterator<Item = io::Result<Token>>;
    fn identifiers(&self) -> &Rc<RefCell<Interner>>;
    fn tokens(&mut self) -> &mut Self::Tokens;
}

impl<I: Iterator<Item = io::Result<char>>> Context for Tokenizer<I> {
    type Tokens = Tokenizer<I>;

    fn tokens(&mut self) -> &mut Self::Tokens {
        self
    }

    fn identifiers(&self) -> &Rc<RefCell<Interner>> {
        Tokenizer::get_interner(self)
    }
}

type VecTokenIter<'a> =
    std::iter::Map<std::iter::Cloned<std::slice::Iter<'a, Token>>, fn(Token) -> io::Result<Token>>;

struct CallContext<'v>(VecTokenIter<'v>, Rc<RefCell<Interner>>);

impl<'v> Context for CallContext<'v> {
    type Tokens = VecTokenIter<'v>;

    fn tokens(&mut self) -> &mut Self::Tokens {
        &mut self.0
    }

    fn identifiers(&self) -> &Rc<RefCell<Interner>> {
        &self.1
    }
}

fn handle_identifier<C: Context>(
    ident: InternedHandle,
    context: &C,
    mut scopes: Rc<Scopes>,
    stack: &mut Stackframe,
    keywords: &Keywords,
) -> io::Result<()> {
    Ok(loop {
        let Scopes(current_scope, parent_scopes) = &*scopes;
        let format_ident = || {
            let mut buf = String::new();
            let interner = context.identifiers().borrow();
            let _ = write!(buf, "{}", interner.lookup(ident));
            buf
        };

        {
            if let Some((
                inner_context,
                inner_scope,
            )) = {
                current_scope.borrow().get(&ident).and_then(|def| {
                    let mut specialization = None;
                    for spec in &def.overloads {
                        if let Ok(bindings) = spec.match_parameters(ident, stack) {
                            specialization = Some((bindings, spec));
                            break;
                        }
                    }
                    if let Some((bindings, spec)) = specialization {
                        let tokens = spec.body.iter().cloned().map(Result::Ok as _);

                        let inner_context = CallContext(tokens, Rc::clone(context.identifiers()));
                        let closure = spec.closure.as_ref().map(Rc::clone);

                        return Some((
                            inner_context,
                            Rc::new(Scopes::with_defs(RefCell::new(bindings), closure)),
                        ))
                    } else {
                        panic!("no matching overload for `{}`", format_ident());
                    }
                })
            } {
                let stackframe = stack.with_offset(stack.len());

                call(
                    inner_context,
                    stackframe,
                    keywords,
                    inner_scope,
                )?;

                break;
            }
        }

        if let Some(outer) = parent_scopes {
            let outer = Rc::clone(outer);
            scopes = outer;
        } else {
            panic!("unknown identifier `{}`", format_ident());
        }
    })
}

fn call<C: Context>(
    mut context: C,
    mut stack: Stackframe,
    keywords: &Keywords,
    scopes: Rc<Scopes>,
) -> io::Result<()> {
    'top_loop: loop {
        let tok = match context.tokens().next() {
            Some(result) => result?,
            None => break,
        };

        match tok {
            Token::Ident(id) if id == keywords.let_ => {
                let mut parameters = vec![];
                for tok in context.tokens() {
                    let tok = tok?;
                    match tok {
                        Token::Ident(id) => parameters.push(Parameter::Argument(id)),
                        Token::Number(val) => parameters.push(Parameter::Value(val)),
                        Token::Char('=') => break,
                        _ => panic!("unexpected token {}", tok),
                    }
                }

                let name = match parameters.pop() {
                    Some(Parameter::Argument(name)) => name,
                    _ => panic!("cannot have unnamed function"),
                };

                let mut body = vec![];

                let mut level = 0;
                for tok in &mut context.tokens() {
                    let tok = tok?;
                    match tok {
                        Token::Char(';') if level == 0 => {
                            let implementation = Implementation {
                                parameters,
                                body,
                                closure: Some(Rc::clone(&scopes)),
                                interner: Rc::clone(context.identifiers()),
                            };

                            let Scopes(current_scope, _) = &*scopes;

                            match current_scope.borrow_mut().entry(name) {
                                Entry::Vacant(entry) => {
                                    entry.insert(Definition {
                                        overloads: vec![implementation],
                                    });
                                }
                                Entry::Occupied(mut entry) => {
                                    entry.get_mut().overloads.push(implementation);
                                }
                            }

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
                println!(
                    "{}",
                    stack
                        .pop()
                        .expect("`print` expected 1 arguments, 0 supplied")
                );
            }
            Token::Number(num) => stack.push(num),
            Token::Char('+') => {
                let a = stack
                    .pop()
                    .expect("operator '+' expected two arguments, 0 supplied");
                let b = stack
                    .pop()
                    .expect("operator '+' expected two arguments, 1 supplied");
                stack.push(a.wrapping_add(b));
            }
            Token::Char('-') => {
                let b = stack
                    .pop()
                    .expect("operator '-' expected two arguments, 0 supplied");
                let a = stack
                    .pop()
                    .expect("operator '-' expected two arguments, 1 supplied");
                stack.push(a.wrapping_sub(b));
            }
            Token::Char('*') => {
                let b = stack
                    .pop()
                    .expect("operator '/' expected two arguments, 0 supplied");
                let a = stack
                    .pop()
                    .expect("operator '/' expected two arguments, 1 supplied");
                stack.push(a * b);
            }
            Token::Char('/') => {
                let b = stack
                    .pop()
                    .expect("operator '/' expected two arguments, 0 supplied");
                let a = stack
                    .pop()
                    .expect("operator '/' expected two arguments, 1 supplied");
                stack.push(a / b);
            }
            Token::Char(ch) => {
                panic!("unexpected character {}", ch);
            }
            Token::Ident(ident) => {
                handle_identifier(ident, &context, Rc::clone(&scopes), &mut stack, keywords)?
            }
        }
    }

    Ok(())
}

struct Keywords {
    let_: InternedHandle,
    print: InternedHandle,
}

fn main() -> io::Result<()> {
    let mut interner = Interner::new();

    let keywords = Keywords {
        print: interner.intern("print"),
        let_: interner.intern("let"),
    };

    let mut stack: Stack = vec![];

    let bytes = io::stdin().bytes();
    let chars = UnsafeDecoder::new(bytes);
    let tokens = Tokenizer::new(chars, interner);

    call(
        tokens,
        Stackframe::from_stack(&mut stack),
        &keywords,
        Rc::new(Scopes::new(None)),
    )?;

    Ok(())
}
