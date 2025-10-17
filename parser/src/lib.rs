use chumsky::{input::ValueInput, prelude::*};
use lexer::*;

pub type Span = SimpleSpan;
pub type Spanned<T> = (T, Span);

#[derive(Clone, Debug)]
pub enum Expr {
    // Literals - values known at compile time
    IntLiteral(i32),      // Changed from i32 to handle larger constants
    FloatLiteral(f32),    // Changed from f32 for better precision
    StringLiteral(String),
    CharLiteral(char),
    
    // Variable access - needs symbol lookup
    Identifier(String),
    
    // Binary/unary operations - use dedicated enums instead of Token
    BinaryOp {
        op: BinaryOperator,  // Changed from Token
        left: Box<Expr>,
        right: Box<Expr>,
    },
    UnaryOp {
        op: UnaryOperator,   // Changed from Token
        operand: Box<Expr>,
    },
    
    // Postfix operations (separate from UnaryOp)
    PostfixOp {
        op: PostfixOperator,
        operand: Box<Expr>,
    },
    
    // Other expression types
    FunctionCall {
        function: Box<Expr>,
        args: Vec<Box<Expr>>,
    },
    ArrayAccess {
        array: Box<Expr>,
        index: Box<Expr>,
    },
    MemberAccess {
        object: Box<Expr>,
        member: String,
    },
    PointerMemberAccess {
        object: Box<Expr>,
        member: String,
    },
    Cast {
        type_name: TypeName,
        expr: Box<Expr>,
    },
    SizeofType(TypeName),
    SizeofExpr(Box<Expr>),
    TernaryOp {
        condition: Box<Expr>,
        then_expr: Box<Expr>,
        else_expr: Box<Expr>,
    },
    Assignment {
        op: AssignmentOperator,
        lvalue: Box<Expr>,
        rvalue: Box<Expr>,
    },
    // Comma operator
    Sequence(Vec<Box<Expr>>),

    CompoundLiteral {
        type_name: TypeName,
        initializers: Vec<Box<Expr>>,
    },
}

// Dedicated operator enums for better type safety and pattern matching
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOperator {
    // Arithmetic
    Add,           // +
    Sub,           // -
    Mul,           // *
    Div,           // /
    Mod,           // %
    
    // Bitwise
    BitwiseAnd,    // &
    BitwiseOr,     // |
    BitwiseXor,    // ^
    ShiftLeft,     // 
    ShiftRight,    // >>
    
    // Logical
    LogicalAnd,    // &&
    LogicalOr,     // ||
    
    // Comparison
    Equal,         // ==
    NotEqual,      // !=
    LessThan,      // 
    LessEqual,     // <=
    GreaterThan,   // >
    GreaterEqual,  // >=
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOperator {
    // Prefix increment/decrement
    PreIncrement,   // ++x
    PreDecrement,   // --x
    
    // Pointer operations
    Dereference,    // *x
    AddressOf,      // &x
    
    // Arithmetic
    Plus,           // +x
    Minus,          // -x
    
    // Bitwise/Logical
    BitwiseNot,     // ~x
    LogicalNot,     // !x
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PostfixOperator {
    Increment,  // x++
    Decrement,  // x--
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AssignmentOperator {
    Assign,        // =
    AddAssign,     // +=
    SubAssign,     // -=
    MulAssign,     // *=
    DivAssign,     // /=
    ModAssign,     // %=
    AndAssign,     // &=
    OrAssign,      // |=
    XorAssign,     // ^=
    ShiftLeftAssign,   // <<=
    ShiftRightAssign,  // >>=
}

// You'll also need a TypeName representation (placeholder for now)
#[derive(Debug, Clone)]
pub struct TypeName {
    // This will eventually contain type specifiers, qualifiers, etc.
    // For now, you could use a simple string or Token
    pub name: String,
}

pub fn parser<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  primary_expression()
    .repeated()
    .collect()
}

// *****************************************************************************************************************************
// Helper parsers for matching Tokens such as RParent, LBrace (needed due to Extras being data in Tokens tagged union)
// TODO find another way to do this?
// *****************************************************************************************************************************
macro_rules! token_parser {
    ($name:ident, $token:path) => {
        fn $name<'tokens, I>() -> impl Parser<'tokens, I, (), extra::Err<chumsky::error::Rich<'tokens, Token, Span>>> + Clone
        where
            I: ValueInput<'tokens, Token = Token, Span = Span>,
        {
            select! {
                $token(_) => (),
            }
        }
    };
}
token_parser!(left_paren, Token::LParen);
token_parser!(right_paren, Token::RParen);
token_parser!(left_bracket, Token::LBracket);
token_parser!(right_bracket, Token::RBracket);
token_parser!(left_brace, Token::LBrace);
token_parser!(right_brace, Token::RBrace);
token_parser!(left_op, Token::LeftOp);
token_parser!(right_op, Token::RightOp);
token_parser!(dot, Token::Dot);
token_parser!(amp, Token::Amp);
token_parser!(star, Token::Star);
token_parser!(plus, Token::Plus);
token_parser!(minus, Token::Minus);
token_parser!(inc, Token::IncOp);
token_parser!(dec, Token::DecOp);
token_parser!(tilde, Token::Tilde);
token_parser!(bang, Token::Bang);
token_parser!(slash, Token::Slash);
token_parser!(percent, Token::Percent);
token_parser!(eq, Token::EqOp);
token_parser!(ne, Token::NeOp);
token_parser!(lt, Token::Lt);
token_parser!(gt, Token::Gt);
token_parser!(lte, Token::LeOp);
token_parser!(gte, Token::GeOp);
token_parser!(caret, Token::Caret);
token_parser!(pipe, Token::Pipe);
token_parser!(question, Token::Question);
token_parser!(colon, Token::Colon);
token_parser!(semicolon, Token::Semicolon);
token_parser!(comma, Token::Comma);
token_parser!(assign, Token::Assign);
token_parser!(ptr_op, Token::PtrOp);

fn identifier<'tokens, I>() -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<chumsky::error::Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    select! {
        Token::Identifier(extra) => (Expr::Identifier(extra.lexeme.clone()), span_from_extra(extra))
    }
}

macro_rules! extract_identifier {
    ($expr:expr) => {
        match $expr {
            Expr::Identifier(name) => name,
            _ => panic!("Expected Expr::Identifier, got {:?}", $expr),
        }
    };
}

// *****************************************************************************************************************************
// Following parsers follow structure and order from https://www.quut.com/c/ANSI-C-grammar-y-2011.html#declarator
// *****************************************************************************************************************************

fn primary_expression<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  choice((
    identifier(),
    constant(),
    string(),
    expression().delimited_by(left_paren(), right_paren())
  ))
}

fn constant<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    select! {
        Token::IntegerConstant(extra) => {
            let value = extra.lexeme.parse::<i32>().unwrap();
            (Expr::IntLiteral(value), span_from_extra(extra))
        },
        Token::FloatConstant(extra) => {
            let value = extra.lexeme.parse::<f32>().unwrap();
            (Expr::FloatLiteral(value), span_from_extra(extra))
        },
    }
}

fn string<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  select! {
    Token::StringLiteral(extra) => (Expr::StringLiteral(extra.clone().lexeme), span_from_extra(extra)),
    // the following is used for __func__ debugging thingy
    // Token::FuncName(extra) => (Expr::(extra.clone().lexeme), span_from_extra(extra)),
  }
}

// TODO fix all the spans that do not include the extra stuff after
pub fn postfix_expression<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  recursive::<_, _, extra::Err<Rich<'tokens, Token, Span>>, _, _>(|postfix| {
    choice((
      primary_expression(),

      // array access
      postfix.clone()
        .then(expression().delimited_by(left_bracket(), right_bracket()))
        .map(|((array, array_span), (index, _index_span))| {
            (Expr::ArrayAccess {
                array: Box::new(array),
                index: Box::new(index),
            }, array_span)
        }),

      // function call w/o arguments
      postfix.clone()
        .then_ignore(left_paren())
        .then_ignore(right_paren())
        .map(|(function, function_span)| (Expr::FunctionCall { 
            function: Box::new(function), 
            args: Vec::new() 
          }, function_span)),

      // function call w/ arguments
      postfix.clone()
        .then(argument_expression_list().delimited_by(left_paren(), right_paren()))
        .map(|((function, function_span), args)| (Expr::FunctionCall { 
            function: Box::new(function), 
            args: args.into_iter().map(|(expr, _span)| Box::new(expr)).collect(),
          }, function_span)),

      // member access
      postfix.clone()
        .then_ignore(dot())
        .then(identifier())
        .map(|((identifier, identifier_span), (member, _member_span))| {
            (Expr::MemberAccess { 
              object: Box::new(identifier), 
              member: extract_identifier!(member) 
            }, identifier_span)
        }),
      
      // pointer member operation
      postfix.clone()
        .then_ignore(ptr_op())
        .then(identifier())
        .map(|((identifier, identifier_span), (member, _member_span))| {
            (Expr::MemberAccess { 
              object: Box::new(identifier), 
              member: extract_identifier!(member) 
            }, identifier_span)
        }),

      // increment operation
      postfix.clone()
        .then_ignore(inc())
        .map(|(identifier, identifier_span)| {
            (Expr::PostfixOp { 
              op: PostfixOperator::Increment,
              operand: Box::new(identifier)
            }, identifier_span)
        }),

      // decrement operation
      postfix.clone()
        .then_ignore(dec())
        .map(|(identifier, identifier_span)| {
            (Expr::PostfixOp { 
              op: PostfixOperator::Decrement,
              operand: Box::new(identifier)
            }, identifier_span)
        }),
      
      // compound literals
      left_paren()
        .ignore_then(type_name())
        .then_ignore(right_paren().then(left_brace()))
        .then(initializer_list())
        .then_ignore(comma().or_not())
        .then_ignore(right_brace())
        .map(|((type_name, type_name_span), initializers)| {
          (Expr::CompoundLiteral { 
            type_name: type_name, 
            initializers: initializers.into_iter().map(|(initializer, _span)| Box::new(initializer)).collect()
          }, type_name_span)
        })
    ))
  })
}

fn argument_expression_list<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Vec<Spanned<Expr>>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    assignment_expression()
      .separated_by(comma())
      .at_least(1)
      .collect()
}

fn unary_expression<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  recursive::<_, _, extra::Err<Rich<'tokens, Token, Span>>, _, _>(|unary| {
    choice((
      postfix_expression(),
      
      // preincrement
      inc()
        .ignore_then(unary.clone())
        .map(|(unary, unary_span)| {
          (Expr::UnaryOp { 
            op: UnaryOperator::PreIncrement, 
            operand: Box::new(unary)
          }, unary_span)
        }),

      // predecrement
      dec()
        .ignore_then(unary.clone())
        .map(|(unary, unary_span)| {
          (Expr::UnaryOp { 
            op: UnaryOperator::PreDecrement, 
            operand: Box::new(unary)
          }, unary_span)
        }),
      
      // cast expression
      unary_operator()
        .then(cast_expression())
        .map(|((unary_op, unary_op_span), (cast, _cast_span))| {
          if let Expr::Cast{type_name, expr} = cast {
            (Expr::UnaryOp { 
              op: unary_op, 
              operand: Box::new(Expr::Cast { 
                type_name: type_name, 
                expr: expr
              })
            }, unary_op_span)
          } else {
            panic!("expected Cast expr!")
          }
        }),

      // TODO SIZEOF and ALIGNOF
    ))
  })
}

fn unary_operator<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<UnaryOperator>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  select! {
    Token::Amp(extra) => (UnaryOperator::AddressOf, span_from_extra(extra)),
    Token::Star(extra) => (UnaryOperator::Dereference, span_from_extra(extra)),
    Token::Plus(extra) => (UnaryOperator::Plus, span_from_extra(extra)),
    Token::Minus(extra) => (UnaryOperator::Minus, span_from_extra(extra)),
    Token::Tilde(extra) => (UnaryOperator::BitwiseNot, span_from_extra(extra)),
    Token::Bang(extra) => (UnaryOperator::LogicalNot, span_from_extra(extra)),
  }
}

fn cast_expression<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  recursive::<_, _, extra::Err<Rich<'tokens, Token, Span>>, _, _>(|cast_expr| {
    choice((
      unary_expression(),
      left_paren()
        .ignore_then(type_name())
        .then_ignore(right_paren())
        .then(cast_expr.clone())
        .map(|((type_name, type_name_span), (cast_expr, _cast_expr_span))| {
          (Expr::Cast { 
            type_name: type_name, 
            expr: Box::new(cast_expr)
          }, type_name_span)
        })
    ))
  })
}


fn multiplicative_expression<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  recursive::<_, _, extra::Err<Rich<'tokens, Token, Span>>, _, _>(|mult| {
    choice((
      cast_expression(),
      mult.clone()
        .then_ignore(star())
        .then(cast_expression())
        .map(|((mult_expr, mult_span), (cast_expr, _cast_span))| {
          (Expr::BinaryOp { 
            op: BinaryOperator::Mul, 
            left: Box::new(mult_expr), 
            right: Box::new(cast_expr) 
          }, mult_span)
        }),
      mult.clone()
        .then_ignore(slash())
        .then(cast_expression())
        .map(|((mult_expr, mult_span), (cast_expr, _cast_span))| {
          (Expr::BinaryOp { 
            op: BinaryOperator::Div, 
            left: Box::new(mult_expr), 
            right: Box::new(cast_expr) 
          }, mult_span)
        }),
      mult.clone()
        .then_ignore(percent())
        .then(cast_expression())
        .map(|((mult_expr, mult_span), (cast_expr, _cast_span))| {
          (Expr::BinaryOp { 
            op: BinaryOperator::Mod, 
            left: Box::new(mult_expr), 
            right: Box::new(cast_expr) 
          }, mult_span)
        }),
    ))
  })
}

fn additive_expression<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  recursive::<_, _, extra::Err<Rich<'tokens, Token, Span>>, _, _>(|additive| {
    choice((
      multiplicative_expression(),
      additive.clone()
        .then_ignore(plus())
        .then(multiplicative_expression())
        .map(|((add_expr, add_span), (mult_expr, _mult_span))| {
          (Expr::BinaryOp { 
            op: BinaryOperator::Add, 
            left: Box::new(add_expr), 
            right: Box::new(mult_expr) 
          }, add_span)
        }),
      additive.clone()
        .then_ignore(minus())
        .then(multiplicative_expression())
        .map(|((add_expr, add_span), (mult_expr, _mult_span))| {
          (Expr::BinaryOp { 
            op: BinaryOperator::Sub, 
            left: Box::new(add_expr), 
            right: Box::new(mult_expr) 
          }, add_span)
        }),
    ))
  })
}

fn shift_expression<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  recursive::<_, _, extra::Err<Rich<'tokens, Token, Span>>, _, _>(|shift| {
    choice((
      additive_expression(),
      shift.clone()
        .then_ignore(left_op())
        .then(additive_expression())
        .map(|((shift_expr, shift_span), (add_expr, _add_span))| {
          (Expr::BinaryOp { 
            op: BinaryOperator::ShiftLeft, 
            left: Box::new(shift_expr), 
            right: Box::new(add_expr) 
          }, shift_span)
        }),
      shift.clone()
        .then_ignore(right_op())
        .then(additive_expression())
        .map(|((shift_expr, shift_span), (add_expr, _add_span))| {
          (Expr::BinaryOp { 
            op: BinaryOperator::ShiftRight, 
            left: Box::new(shift_expr), 
            right: Box::new(add_expr) 
          }, shift_span)
        }),
    ))
  })
}

fn relational_expression<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  recursive::<_, _, extra::Err<Rich<'tokens, Token, Span>>, _, _>(|relational| {
    choice((
      shift_expression(),
      relational.clone()
        .then_ignore(lt())
        .then(shift_expression())
        .map(|((rel_expr, rel_span), (shift_expr, _shift_span))| {
          (Expr::BinaryOp { 
            op: BinaryOperator::LessThan, 
            left: Box::new(rel_expr), 
            right: Box::new(shift_expr) 
          }, rel_span)
        }),
      relational.clone()
        .then_ignore(gt())
        .then(shift_expression())
        .map(|((rel_expr, rel_span), (shift_expr, _shift_span))| {
          (Expr::BinaryOp { 
            op: BinaryOperator::GreaterThan, 
            left: Box::new(rel_expr), 
            right: Box::new(shift_expr) 
          }, rel_span)
        }),
      relational.clone()
        .then_ignore(lte())
        .then(shift_expression())
        .map(|((rel_expr, rel_span), (shift_expr, _shift_span))| {
          (Expr::BinaryOp { 
            op: BinaryOperator::LessEqual, 
            left: Box::new(rel_expr), 
            right: Box::new(shift_expr) 
          }, rel_span)
        }),
      relational.clone()
        .then_ignore(gte())
        .then(shift_expression())
        .map(|((rel_expr, rel_span), (shift_expr, _shift_span))| {
          (Expr::BinaryOp { 
            op: BinaryOperator::GreaterEqual, 
            left: Box::new(rel_expr), 
            right: Box::new(shift_expr) 
          }, rel_span)
        }),
    ))
  })
}

fn equality_expression<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  recursive::<_, _, extra::Err<Rich<'tokens, Token, Span>>, _, _>(|equality| {
    choice((
      relational_expression(),
      equality.clone()
        .then_ignore(eq())
        .then(relational_expression())
        .map(|((eq_expr, eq_span), (rel_expr, _rel_span))| {
          (Expr::BinaryOp { 
            op: BinaryOperator::Equal, 
            left: Box::new(eq_expr), 
            right: Box::new(rel_expr) 
          }, eq_span)
        }),
      equality.clone()
        .then_ignore(ne())
        .then(relational_expression())
        .map(|((eq_expr, eq_span), (rel_expr, _rel_span))| {
          (Expr::BinaryOp { 
            op: BinaryOperator::NotEqual, 
            left: Box::new(eq_expr), 
            right: Box::new(rel_expr) 
          }, eq_span)
        }),
    ))
  })
}

fn and_expression<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{

}

fn exclusive_or_expression<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{

}

fn inclusive_or_expression<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{

}

fn logical_and_expression<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{

}

fn logical_or_expression<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{

}

fn conditional_expression<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{

}

fn assignment_expression<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{

}

fn assignment_operator<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{

}

fn expression<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{

}

fn constant_expression<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{

}

fn declaration<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn declaration_specifiers<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn init_declarator_list<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn init_declarator<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn storage_class_specifier<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn type_specifier<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn struct_or_union_specifier<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn struct_or_union<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn struct_declaration_list<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn struct_declaration<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn specifier_qualifier_list<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn struct_declarator_list<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn struct_declarator<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn enum_specifier<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn enumerator_list<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn enumerator<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn atomic_type_specifier<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn type_qualifier<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn function_specifier<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn alignment_specifier<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn declarator<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn direct_declarator<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn pointer<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn type_qualifier_list<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn parameter_type_list<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn parameter_list<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn parameter_declaration<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn identifier_list<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Vec<Spanned<Expr>>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn type_name<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<TypeName>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn abstract_declarator<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn direct_abstract_declarator<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn initializer<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn initializer_list<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Vec<Spanned<Expr>>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn designation<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn designator_list<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn designator<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn static_assert_declaration<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn statement<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn labeled_statement<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn compound_statement<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn block_item_list<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn block_item<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn expression_statement<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn selection_statement<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn iteration_statement<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn jump_statement<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn translation_unit<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn external_declaration<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn function_definition<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn declaration_list<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn span_from_extra(extra: Extras) -> Span {
  Span::new((), extra.column..extra.column+extra.lexeme.len())
}