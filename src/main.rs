use std::collections::{HashMap, hash_map::Entry};
use std::rc::Rc;
use std::cell::RefCell;
use std::fmt;
use std::fmt::Write;
use std::io;
use std::io::prelude::*;
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
struct Implementation<'a> {
    parameters: Vec<Parameter>,
    body: Vec<Token>,
    closure: Scopes<'a>,
    interner: Rc<RefCell<Interner>>,
}

impl<'a> Implementation<'a> {
    fn match_parameters(
        &self,
        name: InternedHandle,
        stack: &mut Stackframe,
    ) -> Result<HashMap<InternedHandle, Definition>, ()> {
        let format_name = || {
            let mut buf = String::new();
            let interner = self.interner.borrow();
            let _ = write!(buf, "{}", interner.lookup(name));
            buf
        };

        let stack_end_offset = stack
            .len()
            .checked_sub(self.parameters.len())
            .unwrap_or_else(
                || {
                    panic!(
                        "`{}` expected {} arguments, {} supplied",
                        format_name(),
                        self.parameters.len(),
                        stack.len(),
                    );
                }
            );

        let mut taken_args = stack[stack_end_offset..].to_vec();

        let mut bindings = HashMap::new();

        for &param in self.parameters.iter().rev() {
            let stack_value = taken_args.pop().unwrap();

            match param {
                Parameter::Argument(arg_name) => {
                    let implementation = Implementation {
                        parameters: vec![],
                        body: vec![Token::Number(stack_value)],
                        closure: Scopes(HashMap::new(), None),
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

impl<'a> fmt::Display for Implementation<'a> {
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

impl<'a> fmt::Debug for Implementation<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Clone)]
struct Definition<'a> {
    overloads: Vec<Implementation<'a>>,
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

    fn remove(&mut self, count: usize) {
        self.inner.truncate(self.inner.len() - count)
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
                            let Scopes(current_scope, outer_scopes) = &mut scopes;

                            let implementation = Implementation {
                                parameters,
                                body,
                                closure: Scopes(current_scope.clone(), *outer_scopes),
                                interner: Rc::clone(context.identifiers()),
                            };

                            match current_scope.entry(name) {
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
                println!("{}", stack.pop().expect("`print` expected 1 arguments, 0 supplied"));
            }
            Token::Number(num) => stack.push(num),
            Token::Char('+') => {
                let a = stack.pop().expect("operator '+' expected two arguments, 0 supplied");
                let b = stack.pop().expect("operator '+' expected two arguments, 1 supplied");
                stack.push(a.wrapping_add(b));
            }
            Token::Char('-') => {
                let b = stack.pop().expect("operator '-' expected two arguments, 0 supplied");
                let a = stack.pop().expect("operator '-' expected two arguments, 1 supplied");
                stack.push(a.wrapping_sub(b));
            }
            Token::Char(ch) => {
                panic!("unexpected character {}", ch);
            }
            Token::Ident(ident) => {
                let mut scopes = &scopes;
                loop {
                    let format_ident = || {
                        let mut buf = String::new();
                        let interner = context.identifiers().borrow();
                        let _ = write!(buf, "{}", interner.lookup(ident));
                        buf
                    };

                    let Scopes(scope, outer_scopes) = scopes;
                    if let Some(Definition { overloads }) = scope.get(&ident) {
                        if let Some((bindings, spec)) = overloads.iter().find_map(
                            |spec| match spec.match_parameters(ident, &mut stack) {
                                Ok(bindings) => Some((bindings, spec)),
                                Err(()) => None,
                            }
                        ) {
                            let Implementation { body, closure, .. } = spec;

                            let tokens = body
                                .iter()
                                .cloned()
                                .map(Result::Ok as _);

                            let inner_context = CallContext(
                                tokens,
                                Rc::clone(context.identifiers()),
                            );
                            let stackframe = stack.with_offset(stack.len());
                            let scope = Scopes(bindings, Some(closure));

                            call(
                                inner_context,
                                stackframe,
                                keywords,
                                Some(&scope),
                            )?;

                            break;
                        }
                        eprintln!("no matching overload for `{}`", format_ident());
                        for overload in overloads {
                            eprintln!("found {}", overload);
                        }
                        panic!();
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

    call(tokens, Stackframe::from_stack(&mut stack), &keywords, None)?;

    Ok(())
}
