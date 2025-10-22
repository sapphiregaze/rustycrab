pub mod ast;

use ast::*;
use chumsky::input::ValueInput;
use chumsky::prelude::*;
use lexer::*;

pub type Span = SimpleSpan;
pub type Spanned<T> = (T, Span);

pub fn parser<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, TranslationUnit, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    translation_unit().then_ignore(end())
}

// *****************************************************************************
// ************************************************ Helper parsers for matching
// Tokens such as RParent, LBrace (needed due to Extras being data in Tokens
// tagged union) TODO find another way to do this?
// *****************************************************************************
// ************************************************
macro_rules! token_parser {
    ($name:ident, $token:path) => {
        fn $name<'tokens, I>()
        -> impl Parser<'tokens, I, (), extra::Err<chumsky::error::Rich<'tokens, Token, Span>>> + Clone
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
token_parser!(and_op, Token::AndOp);
token_parser!(or_op, Token::OrOp);
token_parser!(caret, Token::Caret);
token_parser!(pipe, Token::Pipe);
token_parser!(question, Token::Question);
token_parser!(colon, Token::Colon);
token_parser!(semicolon, Token::Semicolon);
token_parser!(comma, Token::Comma);
token_parser!(ellipsis, Token::Ellipsis);
token_parser!(assign, Token::Assign);
token_parser!(ptr_op, Token::PtrOp);
token_parser!(if_token, Token::If);
token_parser!(else_token, Token::Else);
token_parser!(switch_token, Token::Switch);
token_parser!(while_token, Token::While);
token_parser!(do_token, Token::Do);
token_parser!(for_token, Token::For);
token_parser!(goto_token, Token::Goto);
token_parser!(continue_token, Token::Continue);
token_parser!(break_token, Token::Break);
token_parser!(return_token, Token::Return);
token_parser!(enum_token, Token::Enum);
token_parser!(case_token, Token::Case);
token_parser!(default_token, Token::Default);
token_parser!(atomic_token, Token::Atomic);
token_parser!(static_token, Token::Static);

fn identifier<'tokens, I>()
-> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<chumsky::error::Rich<'tokens, Token, Span>>> + Clone
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

// *****************************************************************************
// ************************************************ Following parsers follow structure and order from https://www.quut.com/c/ANSI-C-grammar-y-2011.html#declarator
// *****************************************************************************
// ************************************************

static expr: impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone = Recursive::declare();

fn primary_expression<'tokens, 'src: 'tokens, I>(
    expr: impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens
)
-> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    choice((identifier(), constant(), string(),expr.delimited_by(left_paren(), right_paren())))
}

fn constant<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
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

fn enumeration_constant<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    // TODO need to check if it is enumeration constant?
    identifier()
}

fn string<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
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
fn postfix_expression<'tokens, 'src: 'tokens, I>(
    expr: impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
    init: impl Parser<'tokens, I, Spanned<Initializer>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens
)
-> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    // primary expression or compound literal
    let atom = choice((
        primary_expression(expr.clone()),
        // compound literals
        left_paren()
            .ignore_then(type_name(expr.clone(), init.clone()))
            .then_ignore(right_paren().then(left_brace()))
            .then(initializer_list(expr.clone(), init.clone()))
            .then_ignore(comma().or_not())
            .then_ignore(right_brace())
            .map(|((type_name, type_name_span), initializers)| {
                (
                    Expr::CompoundLiteral {
                        type_name: Box::new(type_name),
                        initializers: initializers.into_iter().map(|(initializer, _span)| initializer).collect(),
                    },
                    type_name_span,
                )
            }),
    ));

    // postfix operations that can be applied repeatedly
    let postfix_op = choice((
        // array access
       expr.clone().delimited_by(left_bracket(), right_bracket()).map(|(index, _index_span)| {
            Box::new(move |(array, array_span): Spanned<Expr>| {
                (Expr::ArrayAccess { array: Box::new(array), index: Box::new(index.clone()) }, array_span)
            }) as Box<dyn Fn(Spanned<Expr>) -> Spanned<Expr>>
        }),
        // function call w/o arguments
        left_paren().ignore_then(right_paren()).map(|_| {
            Box::new(|(function, function_span): Spanned<Expr>| {
                (Expr::FunctionCall { function: Box::new(function), args: Vec::new() }, function_span)
            }) as Box<dyn Fn(Spanned<Expr>) -> Spanned<Expr>>
        }),
        // function call w/ arguments
        argument_expression_list(expr.clone(), init.clone()).delimited_by(left_paren(), right_paren()).map(|args| {
            Box::new(move |(function, span): Spanned<Expr>| {
                (
                    Expr::FunctionCall {
                        function: Box::new(function),
                        args: args.iter().map(|(function, _span)| function.clone()).collect(),
                    },
                    span,
                )
            }) as Box<dyn Fn(Spanned<Expr>) -> Spanned<Expr>>
        }),
        // member access
        dot().ignore_then(identifier()).map(|(member, _member_span)| {
            let member_name = extract_identifier!(member);
            Box::new(move |(object, span): Spanned<Expr>| {
                (Expr::MemberAccess { object: Box::new(object), member: member_name.clone() }, span)
            }) as Box<dyn Fn(Spanned<Expr>) -> Spanned<Expr>>
        }),
        // pointer member operation
        ptr_op().ignore_then(identifier()).map(|(member, _member_span)| {
            let member_name = extract_identifier!(member);
            Box::new(move |(object, span): Spanned<Expr>| {
                (Expr::MemberAccess { object: Box::new(object), member: member_name.clone() }, span)
            }) as Box<dyn Fn(Spanned<Expr>) -> Spanned<Expr>>
        }),
        // increment operation
        inc().map(|_| {
            Box::new(|(operand, span): Spanned<Expr>| {
                (Expr::PostfixOp { op: PostfixOperator::Increment, operand: Box::new(operand) }, span)
            }) as Box<dyn Fn(Spanned<Expr>) -> Spanned<Expr>>
        }),
        // decrement operation
        dec().map(|_| {
            Box::new(|(operand, span): Spanned<Expr>| {
                (Expr::PostfixOp { op: PostfixOperator::Decrement, operand: Box::new(operand) }, span)
            }) as Box<dyn Fn(Spanned<Expr>) -> Spanned<Expr>>
        }),
    ));

    // postfix operations repeated using foldl
    atom.foldl(postfix_op.repeated(), |operand, op| op(operand))
}

fn argument_expression_list<'tokens, 'src: 'tokens, I>(
    expr: impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
    init: impl Parser<'tokens, I, Spanned<Initializer>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens
) -> impl Parser<'tokens, I, Vec<Spanned<Expr>>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    assignment_expression(expr.clone(), init.clone()).separated_by(comma()).at_least(1).collect()
}

fn unary_expression<'tokens, 'src: 'tokens, I>(
    expr: impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
    init: impl Parser<'tokens, I, Spanned<Initializer>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    recursive::<_, _, extra::Err<Rich<'tokens, Token, Span>>, _, _>(|unary| {
        choice((
            postfix_expression(expr.clone(), init.clone()),
            // preincrement
            inc().ignore_then(unary.clone()).map(|(unary, unary_span)| {
                (Expr::UnaryOp { op: UnaryOperator::PreIncrement, operand: Box::new(unary) }, unary_span)
            }),
            // predecrement
            dec().ignore_then(unary.clone()).map(|(unary, unary_span)| {
                (Expr::UnaryOp { op: UnaryOperator::PreDecrement, operand: Box::new(unary) }, unary_span)
            }),
            // cast expression
            unary_operator().then(cast_expression(expr.clone(), init.clone())).map(|((unary_op, unary_op_span), (cast, _cast_span))| {
                if let Expr::Cast { type_name, expr } = cast {
                    (
                        Expr::UnaryOp {
                            op: unary_op,
                            operand: Box::new(Expr::Cast { type_name: type_name, expr: expr }),
                        },
                        unary_op_span,
                    )
                } else {
                    panic!("expected Cast expr!")
                }
            }),
            // TODO SIZEOF and ALIGNOF
        ))
    })
}

fn unary_operator<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<UnaryOperator>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
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
    expr: impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
    init: impl Parser<'tokens, I, Spanned<Initializer>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    recursive::<_, _, extra::Err<Rich<'tokens, Token, Span>>, _, _>(|cast_expr| {
        choice((
            unary_expression(expr.clone(), init.clone()),
            left_paren().ignore_then(type_name(expr.clone(), init.clone())).then_ignore(right_paren()).then(cast_expr.clone()).map(
                |((type_name, type_name_span), (cast_expr, _cast_expr_span))| {
                    (Expr::Cast { type_name: Box::new(type_name), expr: Box::new(cast_expr) }, type_name_span)
                },
            ),
        ))
    })
}

fn multiplicative_expression<'tokens, 'src: 'tokens, I>(
    expr: impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
    init: impl Parser<'tokens, I, Spanned<Initializer>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    cast_expression(expr.clone(), init.clone()).foldl(
        choice((star().to(BinaryOperator::Mul), slash().to(BinaryOperator::Div), percent().to(BinaryOperator::Mod)))
            .then(cast_expression(expr.clone(), init.clone()))
            .repeated(),
        |(left_expr, left_span), (op, (right_expr, _right_span))| {
            (Expr::BinaryOp { op, left: Box::new(left_expr), right: Box::new(right_expr) }, left_span)
        },
    )
}

fn additive_expression<'tokens, 'src: 'tokens, I>(
    expr: impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
    init: impl Parser<'tokens, I, Spanned<Initializer>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    multiplicative_expression(expr.clone(), init.clone()).foldl(
        choice((plus().to(BinaryOperator::Add), minus().to(BinaryOperator::Sub)))
            .then(multiplicative_expression(expr.clone(), init.clone()))
            .repeated(),
        |(left_expr, left_span), (op, (right_expr, _right_span))| {
            (Expr::BinaryOp { op, left: Box::new(left_expr), right: Box::new(right_expr) }, left_span)
        },
    )
}

fn shift_expression<'tokens, 'src: 'tokens, I>(
    expr: impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
    init: impl Parser<'tokens, I, Spanned<Initializer>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    additive_expression(expr.clone(), init.clone()).foldl(
        choice((left_op().to(BinaryOperator::ShiftLeft), right_op().to(BinaryOperator::ShiftRight)))
            .then(additive_expression(expr.clone(), init.clone()))
            .repeated(),
        |(left_expr, left_span), (op, (right_expr, _right_span))| {
            (Expr::BinaryOp { op, left: Box::new(left_expr), right: Box::new(right_expr) }, left_span)
        },
    )
}

fn relational_expression<'tokens, 'src: 'tokens, I>(
    expr: impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
    init: impl Parser<'tokens, I, Spanned<Initializer>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    shift_expression(expr.clone(), init.clone()).foldl(
        choice((
            lt().to(BinaryOperator::LessThan),
            gt().to(BinaryOperator::GreaterThan),
            lte().to(BinaryOperator::LessEqual),
            gte().to(BinaryOperator::GreaterEqual),
        ))
        .then(shift_expression(expr.clone(), init.clone()))
        .repeated(),
        |(left_expr, left_span), (op, (right_expr, _right_span))| {
            (Expr::BinaryOp { op, left: Box::new(left_expr), right: Box::new(right_expr) }, left_span)
        },
    )
}

fn equality_expression<'tokens, 'src: 'tokens, I>(
    expr: impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
    init: impl Parser<'tokens, I, Spanned<Initializer>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    relational_expression(expr.clone(), init.clone()).foldl(
        choice((eq().to(BinaryOperator::Equal), ne().to(BinaryOperator::NotEqual)))
            .then(relational_expression(expr.clone(), init.clone()))
            .repeated(),
        |(left_expr, left_span), (op, (right_expr, _right_span))| {
            (Expr::BinaryOp { op, left: Box::new(left_expr), right: Box::new(right_expr) }, left_span)
        },
    )
}

fn and_expression<'tokens, 'src: 'tokens, I>(
    expr: impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
    init: impl Parser<'tokens, I, Spanned<Initializer>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    equality_expression(expr.clone(), init.clone()).foldl(
        amp().to(BinaryOperator::BitwiseAnd).then(equality_expression(expr.clone(), init.clone())).repeated(),
        |(left_expr, left_span), (op, (right_expr, _right_span))| {
            (Expr::BinaryOp { op, left: Box::new(left_expr), right: Box::new(right_expr) }, left_span)
        },
    )
}

fn exclusive_or_expression<'tokens, 'src: 'tokens, I>(
    expr: impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
    init: impl Parser<'tokens, I, Spanned<Initializer>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    and_expression(expr.clone(), init.clone()).foldl(
        caret().to(BinaryOperator::BitwiseXor).then(and_expression(expr.clone(), init.clone())).repeated(),
        |(left_expr, left_span), (op, (right_expr, _right_span))| {
            (Expr::BinaryOp { op, left: Box::new(left_expr), right: Box::new(right_expr) }, left_span)
        },
    )
}

fn inclusive_or_expression<'tokens, 'src: 'tokens, I>(
    expr: impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
    init: impl Parser<'tokens, I, Spanned<Initializer>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    exclusive_or_expression(expr.clone(), init.clone()).foldl(
        pipe().to(BinaryOperator::BitwiseOr).then(exclusive_or_expression(expr.clone(), init.clone())).repeated(),
        |(left_expr, left_span), (op, (right_expr, _right_span))| {
            (Expr::BinaryOp { op, left: Box::new(left_expr), right: Box::new(right_expr) }, left_span)
        },
    )
}

fn logical_and_expression<'tokens, 'src: 'tokens, I>(
    expr: impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
    init: impl Parser<'tokens, I, Spanned<Initializer>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    inclusive_or_expression(expr.clone(), init.clone()).foldl(
        and_op().to(BinaryOperator::LogicalAnd).then(inclusive_or_expression(expr.clone(), init.clone())).repeated(),
        |(left_expr, left_span), (op, (right_expr, _right_span))| {
            (Expr::BinaryOp { op, left: Box::new(left_expr), right: Box::new(right_expr) }, left_span)
        },
    )
}

fn logical_or_expression<'tokens, 'src: 'tokens, I>(
    expr: impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
    init: impl Parser<'tokens, I, Spanned<Initializer>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    logical_and_expression(expr.clone(), init.clone()).foldl(
        or_op().to(BinaryOperator::LogicalOr).then(logical_and_expression(expr.clone(), init.clone())).repeated(),
        |(left_expr, left_span), (op, (right_expr, _right_span))| {
            (Expr::BinaryOp { op, left: Box::new(left_expr), right: Box::new(right_expr) }, left_span)
        },
    )
}

fn conditional_expression<'tokens, 'src: 'tokens, I>(
    expr: impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
    init: impl Parser<'tokens, I, Spanned<Initializer>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    recursive::<_, _, extra::Err<Rich<'tokens, Token, Span>>, _, _>(|cond| {
        choice((
            logical_or_expression(expr.clone(), init.clone()),
            logical_or_expression(expr.clone(), init.clone())
                .then_ignore(question())
                .then(expr)
                .then_ignore(colon())
                .then(cond.clone())
                // NOTE: the names in this map are a bit confusion due to the name of the grammar
                // rules vs member names in the TernaryOp
                .map(|(((or_expr, or_span), (expr, _expr_span)), (cond_expr, _cond_span))| {
                    (
                        Expr::TernaryOp {
                            condition: Box::new(or_expr),
                            then_expr: Box::new(expr),
                            else_expr: Box::new(cond_expr),
                        },
                        or_span,
                    )
                }),
        ))
    })
}

fn assignment_expression<'tokens, 'src: 'tokens, I>(
    expr: impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
    init: impl Parser<'tokens, I, Spanned<Initializer>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    recursive::<_, _, extra::Err<Rich<'tokens, Token, Span>>, _, _>(|assign| {
        choice((
            conditional_expression(expr.clone(), init.clone()),
            unary_expression(expr.clone(), init.clone()).then(assignment_operator()).then(assign.clone()).map(
                |(((cond_expr, cond_span), (assign_op, _assign_op_span)), (assign_expr, _assign_expr_span))| {
                    (
                        Expr::Assignment { op: assign_op, lvalue: Box::new(cond_expr), rvalue: Box::new(assign_expr) },
                        cond_span,
                    )
                },
            ),
        ))
    })
}

fn assignment_operator<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<AssignmentOperator>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    select! {
      Token::EqOp(extra) => (AssignmentOperator::Assign, span_from_extra(extra)),
      Token::AddAssign(extra) => (AssignmentOperator::AddAssign, span_from_extra(extra)),
      Token::SubAssign(extra) => (AssignmentOperator::SubAssign, span_from_extra(extra)),
      Token::MulAssign(extra) => (AssignmentOperator::MulAssign, span_from_extra(extra)),
      Token::DivAssign(extra) => (AssignmentOperator::DivAssign, span_from_extra(extra)),
      Token::ModAssign(extra) => (AssignmentOperator::ModAssign, span_from_extra(extra)),
      Token::AndAssign(extra) => (AssignmentOperator::BitwiseAndAssign, span_from_extra(extra)),
      Token::OrAssign(extra) => (AssignmentOperator::BitwiseOrAssign, span_from_extra(extra)),
      Token::XorAssign(extra) => (AssignmentOperator::BitwiseXorAssign, span_from_extra(extra)),
      Token::LeftAssign(extra) => (AssignmentOperator::ShiftLeftAssign, span_from_extra(extra)),
      Token::RightAssign(extra) => (AssignmentOperator::ShiftRightAssign, span_from_extra(extra)),
    }
}

fn expression<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    recursive(|expr| {
        assignment_expression(expr.clone(), initializer(expr.clone())).separated_by(comma()).at_least(1).collect().map(|list: Vec<Spanned<Expr>>| {
            (Expr::Sequence(list.clone().into_iter().map(|(expr, _span)| Box::new(expr)).collect()), list.clone()[0].1)
        })
    })
}

fn constant_expression<'tokens, 'src: 'tokens, I>(
    expr: impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
    init: impl Parser<'tokens, I, Spanned<Initializer>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    conditional_expression(expr.clone(), init.clone())
}

fn declaration<'tokens, 'src: 'tokens, I>(
    expr: impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
    init: impl Parser<'tokens, I, Spanned<Initializer>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens
)
-> impl Parser<'tokens, I, Spanned<Declaration>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    choice((
        declaration_specifiers(expr.clone(), init.clone()).then_ignore(semicolon()).map(|decl_specifier_list| {
            (
                Declaration {
                    specifiers: decl_specifier_list.clone().into_iter().map(|(specifier, _span)| specifier).collect(),
                    declarators: Vec::new(),
                },
                decl_specifier_list[0].1,
            )
        }),
        declaration_specifiers(expr.clone(), init.clone()).then(init_declarator_list(expr.clone(), init.clone())).then_ignore(semicolon()).map(
            |(decl_specifier_list, declarator_list)| {
                (
                    Declaration {
                        specifiers: decl_specifier_list
                            .clone()
                            .into_iter()
                            .map(|(specifier, _span)| specifier)
                            .collect(),
                        declarators: declarator_list
                            .clone()
                            .into_iter()
                            .map(|(declarator, _span)| declarator)
                            .collect(),
                    },
                    decl_specifier_list[0].1,
                )
            },
        ),
        // TODO maybe implement this
        // static_assert_declaration()
    ))
}

fn declaration_specifiers<'tokens, 'src: 'tokens, I>(
    expr: impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
    init: impl Parser<'tokens, I, Spanned<Initializer>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens
)
-> impl Parser<'tokens, I, Vec<Spanned<DeclarationSpecifier>>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    let mapped_storage_class_specifier = storage_class_specifier()
        .map(|(storage_class, span)| (DeclarationSpecifier::StorageClass(storage_class), span));
    let mapped_type_qualifier =
        type_qualifier().map(|(qualifier, span)| (DeclarationSpecifier::TypeQualifier(qualifier), span));
    let mapped_type_specifier =
        type_specifier(expr.clone(), init.clone()).map(|(specifier, span)| (DeclarationSpecifier::TypeSpecifier(specifier), span));
    let mapped_function_specifier =
        function_specifier().map(|(specifier, span)| (DeclarationSpecifier::FunctionSpecifier(specifier), span));

    mapped_storage_class_specifier
        .or(mapped_type_qualifier)
        .or(mapped_type_specifier)
        .or(mapped_function_specifier)
        .separated_by(comma())
        .collect()
}

fn init_declarator_list<'tokens, 'src: 'tokens, I>(
    expr: impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
    init: impl Parser<'tokens, I, Spanned<Initializer>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens
) -> impl Parser<'tokens, I, Vec<Spanned<InitDeclarator>>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    init_declarator(expr.clone(), init.clone()).separated_by(comma()).at_least(1).collect()
}

fn init_declarator<'tokens, 'src: 'tokens, I>(
    expr: impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
    init: impl Parser<'tokens, I, Spanned<Initializer>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens
) -> impl Parser<'tokens, I, Spanned<InitDeclarator>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    declarator(expr.clone(), init.clone()).map(|(decl, span)| (InitDeclarator { declarator: decl, initializer: Option::None }, span)).or(
        declarator(expr.clone(), init.clone()).then_ignore(eq()).then(init.clone()).map(|((decl, decl_span), (init, _init_span))| {
            (InitDeclarator { declarator: decl, initializer: Some(init) }, decl_span)
        }),
    )
}

fn storage_class_specifier<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<StorageClass>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    select! {
      Token::Auto(extra) => (StorageClass::Auto, span_from_extra(extra)),
      Token::Register(extra) => (StorageClass::Register, span_from_extra(extra)),
      Token::Static(extra) => (StorageClass::Static, span_from_extra(extra)),
      Token::Extern(extra) => (StorageClass::Extern, span_from_extra(extra)),
      Token::Typedef(extra) => (StorageClass::Typedef, span_from_extra(extra)),
    }
}

fn type_specifier<'tokens, 'src: 'tokens, I>(
    expr: impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
    init: impl Parser<'tokens, I, Spanned<Initializer>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens
) -> impl Parser<'tokens, I, Spanned<TypeSpecifier>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    choice((
        select! {
            Token::Void(extra) => (TypeSpecifier::Void, span_from_extra(extra)),
            Token::Char(extra) => (TypeSpecifier::Char, span_from_extra(extra)),
            Token::Short(extra) => (TypeSpecifier::Short, span_from_extra(extra)),
            Token::Int(extra) => (TypeSpecifier::Int, span_from_extra(extra)),
            Token::Long(extra) => (TypeSpecifier::Long, span_from_extra(extra)),
            Token::Float(extra) => (TypeSpecifier::Float, span_from_extra(extra)),
            Token::Double(extra) => (TypeSpecifier::Double, span_from_extra(extra)),
            Token::Signed(extra) => (TypeSpecifier::Signed, span_from_extra(extra)),
            Token::Unsigned(extra) => (TypeSpecifier::Unsigned, span_from_extra(extra)),
            Token::Bool(extra) => (TypeSpecifier::Bool, span_from_extra(extra)),
            Token::Complex(extra) => (TypeSpecifier::Complex, span_from_extra(extra)),
        },
        // struct_or_union_specifier_inner(expr.clone(), init.clone()),
        // enum_specifier_inner(expr.clone(), init.clone()),
    ))
}

fn struct_or_union_specifier<'tokens, 'src: 'tokens, I>(
    expr: impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
    init: impl Parser<'tokens, I, Spanned<Initializer>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens
)
-> impl Parser<'tokens, I, Spanned<TypeSpecifier>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    struct_or_union_specifier_inner(expr.clone(), init.clone())
}

fn struct_or_union_specifier_inner<'tokens, 'src: 'tokens, I>(
    expr: impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
    init: impl Parser<'tokens, I, Spanned<Initializer>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens
) -> impl Parser<'tokens, I, Spanned<TypeSpecifier>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    choice((
        struct_or_union()
            .then_ignore(left_brace())
            .then(struct_declaration_list_inner(expr.clone(), init.clone()))
            .then_ignore(right_brace())
            .map(|((kind, kind_span), struct_declaration_list)| {
                (
                    TypeSpecifier::Struct(StructOrUnion {
                        kind: kind,
                        name: Option::None,
                        declarations: Some(
                            struct_declaration_list.clone().into_iter().map(|(decl, _span)| decl).collect(),
                        ),
                    }),
                    kind_span,
                )
            })
            .boxed(),
        struct_or_union()
            .then(identifier())
            .then_ignore(left_brace())
            .then(struct_declaration_list_inner(expr.clone(), init.clone()))
            .then_ignore(right_brace())
            .map(|(((kind, kind_span), (identifier, _identifier_span)), struct_declaration_list)| {
                (
                    TypeSpecifier::Struct(StructOrUnion {
                        kind: kind,
                        name: Some(extract_identifier!(identifier)),
                        declarations: Some(
                            struct_declaration_list.clone().into_iter().map(|(decl, _span)| decl).collect(),
                        ),
                    }),
                    kind_span,
                )
            })
            .boxed(),
        struct_or_union()
            .then(identifier())
            .map(|((kind, kind_span), (identifier, _identifier_span))| {
                (
                    TypeSpecifier::Struct(StructOrUnion {
                        kind: kind,
                        name: Some(extract_identifier!(identifier)),
                        declarations: Option::None,
                    }),
                    kind_span,
                )
            })
            .boxed(),
    ))
}

fn struct_or_union<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<StructOrUnionKind>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    select! {
      Token::Struct(extra) => (StructOrUnionKind::Struct, span_from_extra(extra)),
      Token::Union(extra) => (StructOrUnionKind::Union, span_from_extra(extra)),
    }
}

fn struct_declaration_list<'tokens, 'src: 'tokens, I>(
    expr: impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
    init: impl Parser<'tokens, I, Spanned<Initializer>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens
)
-> impl Parser<'tokens, I, Vec<Spanned<StructDeclaration>>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    struct_declaration_list_inner(expr.clone(), init.clone())
}

fn struct_declaration_list_inner<'tokens, 'src: 'tokens, I>(
    expr: impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
    init: impl Parser<'tokens, I, Spanned<Initializer>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens
) -> impl Parser<'tokens, I, Vec<Spanned<StructDeclaration>>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    struct_declaration_inner(expr.clone(), init.clone()).repeated().at_least(1).collect()
}

fn struct_declaration<'tokens, 'src: 'tokens, I>(
    expr: impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
    init: impl Parser<'tokens, I, Spanned<Initializer>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens
)
-> impl Parser<'tokens, I, Spanned<StructDeclaration>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    struct_declaration_inner(expr.clone(), init.clone())
}

fn struct_declaration_inner<'tokens, 'src: 'tokens, I>(
    expr: impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
    init: impl Parser<'tokens, I, Spanned<Initializer>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens
) -> impl Parser<'tokens, I, Spanned<StructDeclaration>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    choice((
        specifier_qualifier_list_inner(expr.clone(), init.clone()).then_ignore(semicolon()).map(|specifiers| {
            (
                StructDeclaration {
                    specifiers: specifiers
                        .clone()
                        .into_iter()
                        .filter_map(|(qual_or_spec, _span)| match qual_or_spec {
                            TypeQualOrSpec::Specifier(specifier) => Some(specifier),
                            _ => None,
                        })
                        .collect(),
                    declarators: Vec::new(),
                },
                specifiers.clone()[0].1,
            )
        }),
        specifier_qualifier_list_inner(expr.clone(), init.clone()).then(struct_declarator_list(expr.clone(), init.clone())).then_ignore(semicolon()).map(
            |(specifiers, declarators)| {
                (
                    StructDeclaration {
                        specifiers: specifiers
                            .clone()
                            .into_iter()
                            .filter_map(|(qual_or_spec, _span)| match qual_or_spec {
                                TypeQualOrSpec::Specifier(specifier) => Some(specifier),
                                _ => None,
                            })
                            .collect(),
                        declarators: declarators.into_iter().map(|(declarator, _span)| declarator).collect(),
                    },
                    specifiers.clone()[0].1,
                )
            },
        ),
    ))
}

fn specifier_qualifier_list<'tokens, 'src: 'tokens, I>(
    expr: impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
    init: impl Parser<'tokens, I, Spanned<Initializer>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens
) -> impl Parser<'tokens, I, Vec<Spanned<TypeQualOrSpec>>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    specifier_qualifier_list_inner(expr.clone(), init.clone())
}

fn specifier_qualifier_list_inner<'tokens, 'src: 'tokens, I>(
    expr: impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
    init: impl Parser<'tokens, I, Spanned<Initializer>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens
) -> impl Parser<'tokens, I, Vec<Spanned<TypeQualOrSpec>>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    type_specifier(expr.clone(), init.clone())
        .map(|(specifier, span)| (TypeQualOrSpec::Specifier(specifier), span))
        .or(type_qualifier().map(|(qualifier, span)| (TypeQualOrSpec::Qualifier(qualifier), span)))
        .repeated()
        .at_least(1)
        .collect()
}

fn struct_declarator_list<'tokens, 'src: 'tokens, I>(
    expr: impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
    init: impl Parser<'tokens, I, Spanned<Initializer>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens
)
-> impl Parser<'tokens, I, Vec<Spanned<StructDeclarator>>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    struct_declarator(expr.clone(), init.clone()).separated_by(comma()).at_least(1).collect()
}

fn struct_declarator<'tokens, 'src: 'tokens, I>(
    expr: impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
    init: impl Parser<'tokens, I, Spanned<Initializer>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens
)
-> impl Parser<'tokens, I, Spanned<StructDeclarator>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    declarator(expr.clone(), init.clone()).map(|(declarator, declarator_span)| {
        (StructDeclarator { declarator: declarator, bit_field: Option::None }, declarator_span)
    })

    // TODO implement bit_field thingy
    // colon()
    //   .ignore_then(constant_expression())
    //   .map(|(constant, span)| {
    //     StructDeclarator{
    //       declarator: Declarator { pointer: (), direct_declarator: () },
    //       bit_field: Some(constant)
    //     }
    //   }),
    // declarator()
    //   .then_ignore(colon())
    //   .then(constant_expression())
    //   .map(|((declarator, declarator_span), (constant, constant_span))| {

    //   }),
}

fn enum_specifier<'tokens, 'src: 'tokens, I>(
    expr: impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
    init: impl Parser<'tokens, I, Spanned<Initializer>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens
)
-> impl Parser<'tokens, I, Spanned<TypeSpecifier>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    enum_specifier_inner(expr.clone(), init.clone())
}

fn enum_specifier_inner<'tokens, 'src: 'tokens, I>(
    expr: impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
    init: impl Parser<'tokens, I, Spanned<Initializer>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens
) -> impl Parser<'tokens, I, Spanned<TypeSpecifier>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    choice((
        enum_token().ignore_then(left_brace()).ignore_then(enumerator_list(expr.clone(), init.clone())).then_ignore(right_brace()).map(
            |enumerators| {
                (
                    TypeSpecifier::Enum(EnumSpecifier {
                        name: Option::None,
                        enumerators: Some(
                            enumerators.clone().into_iter().map(|(enumerator, _span)| enumerator).collect(),
                        ),
                    }),
                    enumerators[0].1,
                )
            },
        ),
        enum_token()
            .ignore_then(identifier())
            .then_ignore(left_brace())
            .then(enumerator_list(expr.clone(), init.clone()))
            .then_ignore(right_brace())
            .map(|((identifier, span), enumerators)| {
                (
                    TypeSpecifier::Enum(EnumSpecifier {
                        name: Some(extract_identifier!(identifier)),
                        enumerators: Some(
                            enumerators.clone().into_iter().map(|(enumerator, _span)| enumerator).collect(),
                        ),
                    }),
                    span,
                )
            }),
        enum_token().ignore_then(identifier()).map(|(identifier, span)| {
            (
                TypeSpecifier::Enum(EnumSpecifier {
                    name: Some(extract_identifier!(identifier)),
                    enumerators: Option::None,
                }),
                span,
            )
        }),
    ))
}

fn enumerator_list<'tokens, 'src: 'tokens, I>(
    expr: impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
    init: impl Parser<'tokens, I, Spanned<Initializer>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens
) -> impl Parser<'tokens, I, Vec<Spanned<Enumerator>>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    enumerator(expr.clone(), init.clone()).separated_by(comma()).at_least(1).allow_trailing().collect()
}

fn enumerator<'tokens, 'src: 'tokens, I>(
    expr: impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
    init: impl Parser<'tokens, I, Spanned<Initializer>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens
) -> impl Parser<'tokens, I, Spanned<Enumerator>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    choice((
        enumeration_constant().then_ignore(eq()).then(constant_expression(expr.clone(), init.clone())).map(
            |((enum_constant, enum_span), (constant_expr, _constant_span))| {
                (Enumerator { name: extract_identifier!(enum_constant), value: Some(constant_expr) }, enum_span)
            },
        ),
        enumeration_constant().map(|(enum_constant, enum_span)| {
            (Enumerator { name: extract_identifier!(enum_constant), value: Option::None }, enum_span)
        }),
    ))
}

// TODO implement later
// fn atomic_type_specifier<'tokens, 'src: 'tokens, I>(
// ) -> impl Parser<'tokens, I, Spanned<TypeSpecifier>, extra::Err<Rich<'tokens,
// Token, Span>>> + Clone where
//     I: ValueInput<'tokens, Token = Token, Span = Span>,
// {
//   atomic_token()
//     .ignore_then(left_paren())
//     .ignore_then(type_name())
//     .then_ignore(right_paren())
//     .map(|(type_name, span)| {

//     })
// }

fn type_qualifier<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<TypeQualifier>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    select! {
      Token::Const(extra) => (TypeQualifier::Const, span_from_extra(extra)),
      Token::Restrict(extra) => (TypeQualifier::Restrict, span_from_extra(extra)),
      Token::Volatile(extra) => (TypeQualifier::Volatile, span_from_extra(extra)),
      Token::Atomic(extra) => (TypeQualifier::Atomic, span_from_extra(extra)),
    }
}

fn function_specifier<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<FunctionSpecifier>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    select! {
      Token::Inline(extra) => (FunctionSpecifier::Inline, span_from_extra(extra)),
      Token::Noreturn(extra) => (FunctionSpecifier::NoReturn, span_from_extra(extra)),
    }
}

// TODO implement later
// fn alignment_specifier<'tokens, 'src: 'tokens, I>(
// ) -> impl Parser<'tokens, I, Spanned<>, extra::Err<Rich<'tokens, Token,
// Span>>> + Clone where
//     I: ValueInput<'tokens, Token = Token, Span = Span>,
// {

// }

fn declarator<'tokens, 'src: 'tokens, I>(
    expr: impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
    init: impl Parser<'tokens, I, Spanned<Initializer>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens
)
-> impl Parser<'tokens, I, Spanned<Declarator>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    recursive(|decl| {
        choice((
            pointer().then(direct_declarator(expr.clone(), init.clone(), decl.clone())).map(
                |((pointer, pointer_span), (declarator, _declarator_span))| {
                    (Declarator { pointer: Some(pointer), direct_declarator: declarator }, pointer_span)
                },
            ),
            direct_declarator(expr.clone(), init.clone(), decl.clone()).map(|(declarator, declarator_span)| {
                (Declarator { pointer: Option::None, direct_declarator: declarator }, declarator_span)
            }),
        ))
    })
}

fn direct_declarator<'tokens, 'src: 'tokens, I>(
    expr: impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
    init: impl Parser<'tokens, I, Spanned<Initializer>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
    decl: impl Parser<'tokens, I, Spanned<Declarator>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
) -> impl Parser<'tokens, I, Spanned<DirectDeclarator>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    recursive(|direct| {
        choice((
            identifier()
                .map(|(identifier, span)| (DirectDeclarator::Identifier(extract_identifier!(identifier)), span)),
            decl
                .clone()
                .delimited_by(left_paren(), right_paren())
                .map(|(declarator, span)| (DirectDeclarator::Declarator(Box::new(declarator)), span)),
            direct.clone().then_ignore(left_bracket()).then_ignore(right_bracket()).map(|(declarator, span)| {
                (
                    DirectDeclarator::Array {
                        declarator: Box::new(declarator),
                        array_type: ArrayDeclaratorType::Empty,
                    },
                    span,
                )
            }),
            direct.clone().then_ignore(left_bracket()).then_ignore(star()).then_ignore(right_bracket()).map(
                |(declarator, span)| {
                    (
                        DirectDeclarator::Array {
                            declarator: Box::new(declarator),
                            array_type: ArrayDeclaratorType::VariableLength { qualifiers: Vec::new() },
                        },
                        span,
                    )
                },
            ),
            direct
                .clone()
                .then_ignore(left_bracket())
                .then(choice((
                    static_token().ignore_then(type_qualifier_list()),
                    type_qualifier_list().then_ignore(static_token()),
                )))
                .then(assignment_expression(expr.clone(), init.clone()))
                .then_ignore(right_bracket())
                .map(|(((declarator, declarator_span), type_qualifiers), (expr, _expr_span))| {
                    (
                        DirectDeclarator::Array {
                            declarator: Box::new(declarator),
                            array_type: ArrayDeclaratorType::Static {
                                qualifiers: type_qualifiers
                                    .clone()
                                    .into_iter()
                                    .map(|(qualifier, _span)| qualifier)
                                    .collect(),
                                size: Box::new(expr),
                            },
                        },
                        declarator_span,
                    )
                }),
            direct
                .clone()
                .then_ignore(left_bracket())
                .then_ignore(static_token())
                .then(assignment_expression(expr.clone(), init.clone()))
                .then_ignore(right_bracket())
                .map(|((declarator, declarator_span), (expr, _expr_span))| {
                    (
                        DirectDeclarator::Array {
                            declarator: Box::new(declarator),
                            array_type: ArrayDeclaratorType::Static { qualifiers: Vec::new(), size: Box::new(expr) },
                        },
                        declarator_span,
                    )
                }),
            direct
                .clone()
                .then_ignore(left_bracket())
                .then_ignore(static_token())
                .then(type_qualifier_list())
                .then_ignore(star())
                .then_ignore(right_bracket())
                .map(|((declarator, declarator_span), type_qualifiers)| {
                    (
                        DirectDeclarator::Array {
                            declarator: Box::new(declarator),
                            array_type: ArrayDeclaratorType::VariableLength {
                                qualifiers: type_qualifiers
                                    .clone()
                                    .into_iter()
                                    .map(|(qualifier, _span)| qualifier)
                                    .collect(),
                            },
                        },
                        declarator_span,
                    )
                }),
            direct
                .clone()
                .then_ignore(left_bracket())
                .then(type_qualifier_list())
                .then(assignment_expression(expr.clone(), init.clone()))
                .then_ignore(right_bracket())
                .map(|(((declarator, declarator_span), type_qualifiers), (expr, _expr_span))| {
                    (
                        DirectDeclarator::Array {
                            declarator: Box::new(declarator),
                            array_type: ArrayDeclaratorType::Size {
                                qualifiers: type_qualifiers
                                    .clone()
                                    .into_iter()
                                    .map(|(qualifier, _span)| qualifier)
                                    .collect(),
                                size: Box::new(expr),
                            },
                        },
                        declarator_span,
                    )
                }),
            direct.clone().then_ignore(left_bracket()).then(type_qualifier_list()).then_ignore(right_bracket()).map(
                |((declarator, declarator_span), type_qualifiers)| {
                    (
                        DirectDeclarator::Array {
                            declarator: Box::new(declarator),
                            array_type: ArrayDeclaratorType::Qualifiers(
                                type_qualifiers.clone().into_iter().map(|(qualifier, _span)| qualifier).collect(),
                            ),
                        },
                        declarator_span,
                    )
                },
            ),
            direct.clone().then_ignore(left_bracket()).then(assignment_expression(expr.clone(), init.clone())).then_ignore(right_bracket()).map(
                |((declarator, declarator_span), (expr, _expr_span))| {
                    (
                        DirectDeclarator::Array {
                            declarator: Box::new(declarator),
                            array_type: ArrayDeclaratorType::Size { qualifiers: Vec::new(), size: Box::new(expr) },
                        },
                        declarator_span,
                    )
                },
            ),
            direct.clone().then_ignore(left_paren()).then(parameter_type_list(expr.clone(), init.clone())).then_ignore(right_paren()).map(
                |((declarator, declarator_span), (params, _params_span))| {
                    (
                        DirectDeclarator::Function { declarator: Box::new(declarator), params: Some(params.clone()) },
                        declarator_span,
                    )
                },
            ),
            direct.clone().then_ignore(left_paren()).then_ignore(right_paren()).map(|(declarator, declarator_span)| {
                (DirectDeclarator::Function { declarator: Box::new(declarator), params: Option::None }, declarator_span)
            }),
            direct.clone().then_ignore(left_paren()).then(identifier_list()).then_ignore(right_paren()).map(
                |((declarator, declarator_span), args)| {
                    (
                        DirectDeclarator::FunctionCall {
                            declarator: Box::new(declarator),
                            params: Some(args.clone().into_iter().map(|(expr, _expr_span)| Box::new(expr)).collect()),
                        },
                        declarator_span,
                    )
                },
            ),
        ))
    })
}

fn pointer<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<Pointer>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    recursive::<_, _, extra::Err<Rich<'tokens, Token, Span>>, _, _>(
        |pointer: Recursive<dyn Parser<'_, I, Spanned<Pointer>, extra::Full<Rich<'tokens, Token>, (), ()>>>| {
            star().ignore_then(type_qualifier_list().or_not()).then(pointer.or_not()).map_with(
                |(qualifiers_option, pointer_option), e| {
                    (
                        Pointer {
                            qualifiers: match qualifiers_option {
                                Some(qualifiers) => {
                                    qualifiers.clone().into_iter().map(|(qualifier, _span)| qualifier).collect()
                                }
                                None => Vec::new(),
                            },
                            next: match pointer_option {
                                Some(pointer) => Some(Box::new(pointer.0)),
                                None => None,
                            },
                        },
                        e.span(),
                    )
                },
            )
        },
    )
}

fn type_qualifier_list<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Vec<Spanned<TypeQualifier>>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    type_qualifier().repeated().at_least(1).collect()
}

fn parameter_type_list<'tokens, 'src: 'tokens, I>(
    expr: impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
    init: impl Parser<'tokens, I, Spanned<Initializer>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens
) -> impl Parser<'tokens, I, Spanned<ParameterList>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    parameter_list(expr.clone(), init.clone()).then(comma().then(ellipsis()).or_not()).map_with(|(parameters, variadic), e| {
        (
            ParameterList {
                params: parameters.clone().into_iter().map(|(parameter, _span)| parameter).collect(),
                variadic: match variadic {
                    Some(_) => true,
                    None => false,
                },
            },
            e.span(),
        )
    })
}

fn parameter_list<'tokens, 'src: 'tokens, I>(
    expr: impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
    init: impl Parser<'tokens, I, Spanned<Initializer>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens
) -> impl Parser<'tokens, I, Vec<Spanned<ParameterDeclaration>>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    parameter_declaration(expr.clone(), init.clone()).separated_by(comma()).at_least(1).collect()
}

fn parameter_declaration<'tokens, 'src: 'tokens, I>(
    expr: impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
    init: impl Parser<'tokens, I, Spanned<Initializer>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens
)
-> impl Parser<'tokens, I, Spanned<ParameterDeclaration>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    choice((
        declaration_specifiers(expr.clone(), init.clone()).then(declarator(expr.clone(), init.clone())).map_with(|(specifiers, (declarator, _)), e| {
            (
                ParameterDeclaration {
                    specifiers: specifiers.into_iter().map(|(spec, _)| spec).collect(),
                    declarator: Some(DeclaratorOrAbstract::Declarator(declarator)),
                },
                e.span(),
            )
        }),
        declaration_specifiers(expr.clone(), init.clone()).then(abstract_declarator(expr.clone(), init.clone())).map_with(|(specifiers, (abs_declarator, _)), e| {
            (
                ParameterDeclaration {
                    specifiers: specifiers.into_iter().map(|(spec, _)| spec).collect(),
                    declarator: Some(DeclaratorOrAbstract::Abstract(abs_declarator)),
                },
                e.span(),
            )
        }),
        declaration_specifiers(expr.clone(), init.clone()).map_with(|specifiers, e| {
            (
                ParameterDeclaration {
                    specifiers: specifiers.into_iter().map(|(spec, _)| spec).collect(),
                    declarator: None,
                },
                e.span(),
            )
        }),
    ))
}

fn identifier_list<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Vec<Spanned<Expr>>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    identifier().separated_by(comma()).at_least(1).collect()
}

fn type_name<'tokens, 'src: 'tokens, I>(
    expr: impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
    init: impl Parser<'tokens, I, Spanned<Initializer>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens
) -> impl Parser<'tokens, I, Spanned<TypeName>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    specifier_qualifier_list(expr.clone(), init.clone()).then(abstract_declarator(expr.clone(), init.clone()).or_not()).map_with(
        |(specifiers_qualifiers, declarator_option), e| {
            let specifiers = specifiers_qualifiers
                .clone()
                .into_iter()
                .filter_map(|(specifier_qualifier, _span)| match specifier_qualifier {
                    TypeQualOrSpec::Specifier(specifier) => Some(specifier),
                    _ => None,
                })
                .collect();

            let qualifiers = specifiers_qualifiers
                .clone()
                .into_iter()
                .filter_map(|(specifier_qualifier, _span)| match specifier_qualifier {
                    TypeQualOrSpec::Qualifier(qualifier) => Some(qualifier),
                    _ => None,
                })
                .collect();

            (
                TypeName {
                    specifiers: specifiers,
                    qualifiers: qualifiers,
                    declarator: match declarator_option {
                        Some(declarator) => Some(declarator.0),
                        _ => None,
                    },
                },
                e.span(),
            )
        },
    )
}

fn abstract_declarator<'tokens, 'src: 'tokens, I>(
    expr: impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
    init: impl Parser<'tokens, I, Spanned<Initializer>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens
)
-> impl Parser<'tokens, I, Spanned<AbstractDeclarator>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    pointer().or_not().then(direct_abstract_declarator(expr.clone(), init.clone()).or_not()).map_with(|(pointer_option, declarator_option), e| {
        (
            AbstractDeclarator {
                pointer: pointer_option.map(|pointer| pointer.0),
                direct: declarator_option.map(|declarator| declarator.0),
            },
            e.span(),
        )
    })
}

fn direct_abstract_declarator<'tokens, 'src: 'tokens, I>(
    expr: impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
    init: impl Parser<'tokens, I, Spanned<Initializer>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens
) -> impl Parser<'tokens, I, Spanned<DirectAbstractDeclarator>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    recursive(|direct_abstract_declarator| {
        // define the abstract_declarator parser inside the recursive scope
        let abstract_declarator = pointer()
            .map(|(p, _span)| p)
            .or_not()
            .then(direct_abstract_declarator.clone().or_not())
            .map(|(pointer, direct)| AbstractDeclarator { pointer, direct: direct.map(|(d, _span)| d) });

        let base = abstract_declarator
            .delimited_by(left_paren(), right_paren())
            .map_with(|abs_decl, e| (DirectAbstractDeclarator::Declarator(Box::new(abs_decl)), e.span()));

        // parser for array suffixes
        let array_suffix = left_bracket()
            .ignore_then(choice((
                // [ static qualifiers? expr ] or [ qualifiers? static expr ]
                choice((
                    static_token().ignore_then(type_qualifier_list().or_not()),
                    type_qualifier_list().or_not().then_ignore(static_token()),
                ))
                .then(assignment_expression(expr.clone(), init.clone()))
                .map(|(qualifiers_opt, (expr, _))| ArrayDeclaratorType::Static {
                    qualifiers: qualifiers_opt.map_or(Vec::new(), |qs| qs.into_iter().map(|(q, _)| q).collect()),
                    size: Box::new(expr),
                }),
                // [ * ]
                star().to(ArrayDeclaratorType::VariableLength { qualifiers: Vec::new() }),
                // generic case: [ qualifiers? expr? ]
                type_qualifier_list().or_not().then(assignment_expression(expr.clone(), init.clone()).or_not()).map(
                    |(qualifiers_opt, expr_opt)| {
                        let qualifiers =
                            qualifiers_opt.map_or(Vec::new(), |qs| qs.into_iter().map(|(q, _)| q).collect());
                        match (expr_opt, qualifiers.is_empty()) {
                            (Some((expr, _)), false) => ArrayDeclaratorType::Size { qualifiers, size: Box::new(expr) },
                            (Some((expr, _)), true) => {
                                ArrayDeclaratorType::Size { qualifiers: Vec::new(), size: Box::new(expr) }
                            }
                            (None, false) => ArrayDeclaratorType::Qualifiers(qualifiers),
                            (None, true) => ArrayDeclaratorType::Empty,
                        }
                    },
                ),
            )))
            .then_ignore(right_bracket());

        // parser for function suffixes
        let function_suffix = parameter_list(expr.clone(), init.clone()).delimited_by(left_paren(), right_paren());
        let suffix = choice((
            array_suffix
                .map_with(|array_type, e| (DirectAbstractDeclarator::Array { declarator: None, array_type }, e.span())),
            function_suffix.map_with(|params, e| {
                let param_list = ParameterList {
                    params: params.into_iter().map(|(p, _span)| p).collect(),
                    variadic: false, // TODO: add parsing for '...' to set this correctly
                };
                (DirectAbstractDeclarator::Function { declarator: None, params: Some(param_list) }, e.span())
            }),
        ));

        base.or_not()
            .foldl(suffix.repeated(), |acc, (mut wrapper, wrapper_span)| {
                let new_span = acc
                    .as_ref()
                    .map(|(_d, span): &(DirectAbstractDeclarator, Span)| span.union(wrapper_span))
                    .unwrap_or(wrapper_span);

                match &mut wrapper {
                    DirectAbstractDeclarator::Array { declarator, .. } => {
                        *declarator = acc.map(|(d, _)| Box::new(d));
                    }
                    DirectAbstractDeclarator::Function { declarator, .. } => {
                        *declarator = acc.map(|(d, _)| Box::new(d));
                    }
                    _ => unreachable!(),
                }

                Some((wrapper, new_span))
            })
            .try_map(|opt, span| opt.ok_or_else(|| Rich::custom(span, "Expected a direct abstract declarator")))
    })
}

fn initializer<'tokens, 'src: 'tokens, I>(
    expr: impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
) -> impl Parser<'tokens, I, Spanned<Initializer>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    recursive(|initializer| {
        choice((
            initializer_list(expr.clone(), initializer.clone()).map_with(|list, e| {
                (Initializer::List(list.clone().into_iter().map(|(item, _span)| item).collect()), e.span())
            }),
            assignment_expression(expression(), initializer).map_with(|(expr, _expr_span), e| (Initializer::Expr(expr), e.span())),
        ))
    })
}

fn initializer_list<'tokens, 'src: 'tokens, I>(
    expr: impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
    init: impl Parser<'tokens, I, Spanned<Initializer>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
) -> impl Parser<'tokens, I, Vec<Spanned<InitializerListItem>>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    designation(expr.clone(), init.clone()).or_not().then(init.clone()).separated_by(comma()).allow_trailing().at_least(1).collect().map(
        |list: Vec<(Option<(Designation, SimpleSpan)>, (Initializer, SimpleSpan))>| {
            list.clone()
                .into_iter()
                .map(|(designation_option, (init, init_span))| {
                    (
                        InitializerListItem {
                            designation: match designation_option {
                                Some(designation) => Some(designation.0),
                                _ => None,
                            },
                            initializer: init,
                        },
                        init_span,
                    ) // TODO this needs to somehow also include designation span
                })
                .collect()
        },
    )
}

fn designation<'tokens, 'src: 'tokens, I>(
    expr: impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
    init: impl Parser<'tokens, I, Spanned<Initializer>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens
) -> impl Parser<'tokens, I, Spanned<Designation>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    designator_list(expr.clone(), init.clone()).then_ignore(eq()).map_with(|list, e| {
        (
            Designation { designators: list.clone().into_iter().map(|(designator, _span)| designator).collect() },
            e.span(),
        )
    })
}

fn designator_list<'tokens, 'src: 'tokens, I>(
    expr: impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
    init: impl Parser<'tokens, I, Spanned<Initializer>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens
) -> impl Parser<'tokens, I, Vec<Spanned<Designator>>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    designator(expr.clone(), init.clone()).repeated().at_least(1).collect()
}

fn designator<'tokens, 'src: 'tokens, I>(
    expr: impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
    init: impl Parser<'tokens, I, Spanned<Initializer>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens
) -> impl Parser<'tokens, I, Spanned<Designator>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    choice((
        constant_expression(expr.clone(), init.clone())
            .delimited_by(left_bracket(), right_bracket())
            .map_with(|(expr, _span), e| (Designator::Index(expr), e.span())),
        dot()
            .ignore_then(identifier())
            .map_with(|(identifier, _span), e| (Designator::Member(extract_identifier!(identifier)), e.span())),
    ))
}

// TODO implement later
// fn static_assert_declaration<'tokens, 'src: 'tokens, I>(
// ) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token,
// Span>>> + Clone where
//     I: ValueInput<'tokens, Token = Token, Span = Span>,
// {

// }

fn statement<'tokens, 'src: 'tokens, I>(
    expr: impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
    init: impl Parser<'tokens, I, Spanned<Initializer>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
) -> impl Parser<'tokens, I, Spanned<Stmt>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    recursive(|statement| {
        choice((
            labeled_statement(expr.clone(), init.clone(), statement.clone()),
            compound_statement(expr.clone(), init.clone()),
            expression_statement(expr.clone()),
            selection_statement(expr.clone(), statement.clone()),
            iteration_statement(expr.clone(), init.clone(), statement.clone()),
            jump_statement(expr.clone()),
        ))
    })
}

fn labeled_statement<'tokens, 'src: 'tokens, I>(
    expr: impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
    init: impl Parser<'tokens, I, Spanned<Initializer>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
    statement: impl Parser<'tokens, I, Spanned<Stmt>, extra::Err<Rich<'tokens, Token, Span>>> + Clone,
) -> impl Parser<'tokens, I, Spanned<Stmt>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    choice((
        identifier().then_ignore(colon()).then(statement.clone()).map_with(
            |((identifier, _identifier_span), (stmt, _stmt_span)), e| {
                (Stmt::Labeled { label: extract_identifier!(identifier), stmt: Box::new(stmt) }, e.span())
            },
        ),
        case_token().ignore_then(constant_expression(expr.clone(), init.clone())).then_ignore(colon()).then(statement.clone()).map_with(
            |((constant, _constant_span), (stmt, _stmt_span)), e| {
                (Stmt::Case { expr: constant, stmt: Box::new(stmt) }, e.span())
            },
        ),
        default_token()
            .ignore_then(colon())
            .ignore_then(statement.clone())
            .map_with(|(stmt, _stmt_span), e| (Stmt::Default(Box::new(stmt)), e.span())),
    ))
}

fn compound_statement<'tokens, 'src: 'tokens, I>(
    expr: impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
    init: impl Parser<'tokens, I, Spanned<Initializer>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
)
-> impl Parser<'tokens, I, Spanned<Stmt>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    block_item_list(expr.clone(), init.clone()).or_not().delimited_by(left_brace(), right_brace()).map_with(|list_option, e| {
        (
            Stmt::Compound(CompoundStmt {
                items: match list_option {
                    Some(list) => list.clone().into_iter().map(|(item, _span)| item).collect(),
                    _ => Vec::new(),
                },
            }),
            e.span(),
        )
    })
}

fn block_item_list<'tokens, 'src: 'tokens, I>(
    expr: impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
    init: impl Parser<'tokens, I, Spanned<Initializer>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
)
-> impl Parser<'tokens, I, Vec<Spanned<BlockItem>>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    block_item(expr.clone(), init.clone()).repeated().at_least(1).collect()
}

fn block_item<'tokens, 'src: 'tokens, I>(
    expr: impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
    init: impl Parser<'tokens, I, Spanned<Initializer>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
)
-> impl Parser<'tokens, I, Spanned<BlockItem>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    choice((
        declaration(expr.clone(), init.clone()).map_with(|(declaration, _declaration_span), e| (BlockItem::Decl(declaration), e.span())),
        statement(expr.clone(), init.clone()).map_with(|(statement, _statement_span), e| (BlockItem::Stmt(statement), e.span())),
    ))
}

fn expression_statement<'tokens, 'src: 'tokens, I>(
    expr: impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone,
) -> impl Parser<'tokens, I, Spanned<Stmt>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
   expr.clone()
        .or_not()
        .then_ignore(semicolon())
        .map_with(|expr_option, e| (Stmt::Expr(expr_option.map(|expr| expr.0)), e.span()))
}

fn selection_statement<'tokens, 'src: 'tokens, I>(
    expr: impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone,
    statement: impl Parser<'tokens, I, Spanned<Stmt>, extra::Err<Rich<'tokens, Token, Span>>> + Clone,
) -> impl Parser<'tokens, I, Spanned<Stmt>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    choice((
        if_token()
            .ignore_then(expr.clone().delimited_by(left_paren(), right_paren()))
            .then(statement.clone())
            .then(else_token().ignore_then(statement.clone()).or_not())
            .map_with(|(((expr, _expr_span), (then_stmt, _then_stmt_span)), else_option), e| {
                (
                    Stmt::If {
                        condition: expr,
                        then_stmt: Box::new(then_stmt),
                        else_stmt: match else_option {
                            Some((else_stmt, _else_span)) => Some(Box::new(else_stmt)),
                            _ => None,
                        },
                    },
                    e.span(),
                )
            }),
        switch_token()
            .ignore_then(expr.clone().delimited_by(left_paren(), right_paren()))
            .then(statement.clone())
            .map_with(|((expr, _expr_span), (stmt, _stmt_span)), e| {
                (Stmt::Switch { condition: expr, body: Box::new(stmt) }, e.span())
            }),
    ))
}

fn iteration_statement<'tokens, 'src: 'tokens, I>(
    expr: impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
    init: impl Parser<'tokens, I, Spanned<Initializer>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
    statement: impl Parser<'tokens, I, Spanned<Stmt>, extra::Err<Rich<'tokens, Token, Span>>> + Clone,
) -> impl Parser<'tokens, I, Spanned<Stmt>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    choice((
        while_token()
            .ignore_then(expr.clone().delimited_by(left_paren(), right_paren()))
            .then(statement.clone())
            .map_with(|((expr, _expr_span), (stmt, _stmt_span)), e| {
                (Stmt::While { condition: expr, body: Box::new(stmt) }, e.span())
            }),
        do_token()
            .ignore_then(statement.clone())
            .then_ignore(while_token())
            .then(expr.clone().delimited_by(left_paren(), right_paren()))
            .then_ignore(semicolon())
            .map_with(|((stmt, _stmt_span), (expr, _expr_span)), e| {
                (Stmt::DoWhile { body: Box::new(stmt), condition: expr }, e.span())
            }),
        for_token()
            .ignore_then(expression_statement(expr.clone()).then(expression_statement(expr.clone())).then(expr.clone().or_not()))
            .delimited_by(left_paren(), right_paren())
            .then(statement.clone())
            .map_with(
                |(
                    (((init_stmt, _init_stmt_span), (cond_stmt, _cond_stmt_span)), update_expr_option),
                    (stmt, _stmt_span),
                ),
                 e| {
                    (
                        Stmt::For {
                            init: Some(ForInit::Expr(match init_stmt {
                                Stmt::Expr(expr) => expr.unwrap(),
                                _ => panic!("expected expression for loop initializer!"),
                            })),
                            condition: match cond_stmt {
                                Stmt::Expr(expr) => expr,
                                _ => panic!("expected expression for loop condition!"),
                            },
                            increment: update_expr_option.map(|option| option.0),
                            body: Box::new(stmt),
                        },
                        e.span(),
                    )
                },
            ),
        for_token()
            .ignore_then(declaration(expr.clone(), init.clone()).then(expression_statement(expr.clone())).then(expr.clone().or_not()))
            .delimited_by(left_paren(), right_paren())
            .then(statement.clone())
            .map_with(
                |(
                    (((init_decl, _init_decl_span), (cond_stmt, _cond_stmt_span)), update_expr_option),
                    (stmt, _stmt_expr),
                ),
                 e| {
                    (
                        Stmt::For {
                            init: Some(ForInit::Decl(init_decl)),
                            condition: match cond_stmt {
                                Stmt::Expr(expr) => expr,
                                _ => panic!("expected expression for loop condition!"),
                            },
                            increment: update_expr_option.map(|option| option.0),
                            body: Box::new(stmt),
                        },
                        e.span(),
                    )
                },
            ),
    ))
}

fn jump_statement<'tokens, 'src: 'tokens, I>(
    expr: impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone,
) -> impl Parser<'tokens, I, Spanned<Stmt>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    choice((
        goto_token()
            .ignore_then(identifier())
            .then_ignore(semicolon())
            .map_with(|(identifier, _span), e| (Stmt::Goto(extract_identifier!(identifier)), e.span())),
        continue_token().then(semicolon()).ignored().map_with(|_, e| (Stmt::Continue, e.span())),
        break_token().then(semicolon()).ignored().map_with(|_, e| (Stmt::Break, e.span())),
        return_token()
            .ignore_then(expr.clone().or_not())
            .then_ignore(semicolon())
            .map_with(|expr_option, e| (Stmt::Return(expr_option.map(|expr| expr.0)), e.span())),
    ))
}

/// A translation unit (C source file) is a list of external declarations
pub type TranslationUnit = Vec<Spanned<ExternalDecl>>;
fn translation_unit<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, TranslationUnit, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    // define expr recursive parser
    expr.define(
        assignment_expression(expr.clone(), initializer(expr.clone())).separated_by(comma()).at_least(1).collect().map(|list: Vec<Spanned<Expr>>| {
            (Expr::Sequence(list.clone().into_iter().map(|(expr_item, _span)| Box::new(expr_item)).collect()), list.clone()[0].1)
        })
    );

    external_declaration(initializer(expr)).repeated().at_least(1).collect()
}

fn external_declaration<'tokens, 'src: 'tokens, I>(
    init: impl Parser<'tokens, I, Spanned<Initializer>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
) -> impl Parser<'tokens, I, Spanned<ExternalDecl>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    choice((
        function_definition(expr, init.clone()).map_with(|(definition, _span), e| (ExternalDecl::FuncDef(definition), e.span())),
        declaration(expr, init.clone()).map_with(|(declaration, _span), e| (ExternalDecl::Decl(declaration), e.span())),
    ))
}

fn function_definition<'tokens, 'src: 'tokens, I>(
    expr: impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
    init: impl Parser<'tokens, I, Spanned<Initializer>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
) -> impl Parser<'tokens, I, Spanned<FunctionDefinition>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    declaration_specifiers(expr.clone(), init.clone()).then(declarator(expr.clone(), init.clone())).then(declaration_list(expr.clone(), init.clone()).or_not()).then(compound_statement(expr.clone(), init.clone())).map_with(
        |(((specifiers, (declarator, _declarator_span)), declarations_option), (compound, _compound_span)), e| {
            (
                FunctionDefinition {
                    specifiers: specifiers.clone().into_iter().map(|(specifier, _span)| specifier).collect(),
                    declarator: declarator,
                    declarations: match declarations_option {
                        Some(declarations) => {
                            declarations.into_iter().map(|(declaration, _span)| declaration).collect()
                        }
                        _ => Vec::new(),
                    },
                    body: match compound {
                        Stmt::Compound(stmt) => stmt,
                        _ => CompoundStmt { items: Vec::new() },
                    },
                },
                e.span(),
            )
        },
    )
}

fn declaration_list<'tokens, 'src: 'tokens, I>(
    expr: impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
    init: impl Parser<'tokens, I, Spanned<Initializer>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
)
-> impl Parser<'tokens, I, Vec<Spanned<Declaration>>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    declaration(expr.clone(), init.clone()).repeated().at_least(1).collect()
}

fn span_from_extra(extra: Extras) -> Span {
    Span::new((), extra.column..extra.column + extra.lexeme.len())
}
