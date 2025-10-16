use chumsky::{input::ValueInput, prelude::*};
use lexer::*;

pub type Span = SimpleSpan;
pub type Spanned<T> = (T, Span);

#[derive(Clone, Debug)]
pub enum Node {
  Identifier(String),
  IntegerConstant(i32),
  FloatConstant(f32),
  StringLiteral(String),
  FuncName(String),
  Sizeof,
  TypedefName,
  EnumerationConstant(String),
  Typedef,
  Extern,
  Static,
  Auto,
  Register,
  Inline,
  Const,
  Restrict,
  Volatile,
  Bool,
  Char,
  Short,
  Int,
  Long,
  Signed,
  Unsigned,
  Float,
  Double,
  Void,
  Complex,
  Imaginary,
  Struct,
  Union,
  Enum,
  Ellipsis,
  Case,
  Default,
  If,
  Else,
  Switch,
  While,
  Do,
  For,
  Goto,
  Continue,
  Break,
  Return,
  Alignas,
  Alignof,
  Atomic,
  Generic,
  Noreturn,
  StaticAssert,
  ThreadLocal,
}

pub fn parser<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Vec<Spanned<Node>>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  constant().repeated().collect()
}

fn constant<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Node>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  // TODO make these lines a lot shorter
  select! {
    Token::IntegerConstant(extra) => (Node::IntegerConstant(extra.lexeme.parse::<i32>().unwrap()), Span::new((), extra.column..extra.column+extra.lexeme.len())),
    Token::FloatConstant(extra) => (Node::FloatConstant(extra.lexeme.parse::<f32>().unwrap()), Span::new((), extra.column..extra.column+extra.lexeme.len())),
    // TODO find some way to do a type check to see if identifier is enum constant?
    Token::Identifier(extra) => (Node::EnumerationConstant(extra.lexeme.clone()), Span::new((), extra.column..extra.column+extra.lexeme.len())),
  }
}