use lexer::Token as LexerToken;
use lexer::{Extras};

use ariadne::{Color, Label, Report, ReportKind, Source};
use chumsky::{
    input::{Stream, ValueInput},
    span::SimpleSpan,
    prelude::*,
};

#[derive(Debug)]
pub struct Analyzer {
  inner: Vec<LexerToken>,
}

impl Analyzer {
  pub fn new(inner: Vec<LexerToken>) -> Self {
    Self { inner }
  }

  // pub fn parse(&self) -> Result<SExpr, Vec<extra::Err<Rich<'_, LexerToken>>>> {
  pub fn parse(&self) -> Result<SExpr, String> {
    let token_stream = Stream::from_iter(
        self.inner
            .iter()
            .cloned()
            .map(|t| (t, std::ops::Range::default().into())), // Dummy spans
    )
    .map((0..0).into(), |(t, s): (_, _)| (t, s));

    // Parse the token stream with our chumsky parser
    match parser().parse(token_stream).into_result() {
      // If parsing was successful, attempt to evaluate the s-expression
      Ok(sexpr) => match sexpr.eval() {
          Ok(out) => {
              println!("Result = {out}");
              Ok(sexpr)
          },
          Err(err) => {
              println!("Runtime error: {err}");
              Ok(sexpr)
          },
      },
      // If parsing was unsuccessful, generate a nice user-friendly diagnostic with ariadne. You could also use
      // codespan, or whatever other diagnostic library you care about. You could even just display-print the errors
      // with Rust's built-in `Display` trait, but it's a little crude
      Err(errs) => {
          for err in errs {
            Report::build(ReportKind::Error, ((), err.span().into_range()))
              .with_config(ariadne::Config::new().with_index_type(ariadne::IndexType::Byte))
              .with_code(3)
              .with_message(err.to_string())
              .with_label(
                Label::new(((), err.span().into_range()))
                  .with_message(err.reason().to_string())
                  .with_color(Color::Red),
              )
              .finish()
              .eprint(Source::from(self.inner.iter().map(|t| t.get_lexeme()).collect::<String>()))
              .unwrap();
            }
          return Err(String::from("Parsing errors occurred"));
      }
    }
  }

}

#[derive(Debug)]
pub enum SExpr {
  Float(f64),
  Add,
  Sub,
  Mul,
  Div,
  List(Vec<Self>),
}

// This function signature looks complicated, but don't fear! We're just saying that this function is generic over
// inputs that:
//     - Can have tokens pulled out of them by-value, by cloning (`ValueInput`)
//     - Gives us access to slices of the original input (`SliceInput`)
//     - Produces tokens of type `Token`, the type we defined above (`Token = Token<'a>`)
//     - Produces spans of type `SimpleSpan`, a built-in span type provided by chumsky (`Span = SimpleSpan`)
// The function then returns a parser that:
//     - Has an input type of type `I`, the one we declared as a type parameter
//     - Produces an `SExpr` as its output
//     - Uses `Rich`, a built-in error type provided by chumsky, for error generation

fn parser<'tokens, 'src: 'tokens, I>(
) -> impl chumsky::Parser<'tokens, I, SExpr, extra::Err<Rich<'tokens, LexerToken>>>
where
    I: ValueInput<'tokens, Token = LexerToken, Span = SimpleSpan>,
{
    recursive(|sexpr| {
        let atom = select! {
            LexerToken::IntegerConstant(x) => SExpr::Float(x.lexeme.parse().unwrap()),
            LexerToken::FloatConstant(x) => SExpr::Float(x.lexeme.parse().unwrap()),
            LexerToken::Plus(_) => SExpr::Add,
            LexerToken::Minus(_) => SExpr::Sub,
            LexerToken::Star(_) => SExpr::Mul,
            LexerToken::Slash(_) => SExpr::Div,
        };

        let list = sexpr
            .repeated()
            .collect()
            .map(SExpr::List)
            .delimited_by(just(LexerToken::LParen(Extras::default())), just(LexerToken::RParen(Extras::default())));

        atom.or(list)
    })
}

impl SExpr {
    // Recursively evaluate an s-expression
    fn eval(&self) -> Result<f64, &'static str> {
        match self {
            Self::Float(x) => Ok(*x),
            Self::Add => Err("Cannot evaluate operator '+'"),
            Self::Sub => Err("Cannot evaluate operator '-'"),
            Self::Mul => Err("Cannot evaluate operator '*'"),
            Self::Div => Err("Cannot evaluate operator '/'"),
            Self::List(list) => match &list[..] {
                [Self::Add, tail @ ..] => tail.iter().map(SExpr::eval).sum(),
                [Self::Mul, tail @ ..] => tail.iter().map(SExpr::eval).product(),
                [Self::Sub, init, tail @ ..] => {
                    Ok(init.eval()? - tail.iter().map(SExpr::eval).sum::<Result<f64, _>>()?)
                }
                [Self::Div, init, tail @ ..] => {
                    Ok(init.eval()? / tail.iter().map(SExpr::eval).product::<Result<f64, _>>()?)
                }
                _ => Err("Cannot evaluate list"),
            },
        }
    }
}