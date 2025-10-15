use chumsky::prelude::*;

#[derive(Clone, Debug)]
pub enum Values {
  Okay,
  Invalid
}

pub fn parser<'src>() -> impl Parser<'src, &'src str, Values> {
  just("test").to(Values::Okay).then_ignore(empty())
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_parser() {
    assert!(parser().parse("test").into_result().is_ok());
    assert!(parser().parse("testtest").into_result().is_err());
  }
}