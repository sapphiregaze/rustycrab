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

fn primary_expression<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    choice((identifier(), constant(), string(), expression().delimited_by(left_paren(), right_paren())))
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
// TODO left recursion problem
pub fn postfix_expression<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    recursive::<_, _, extra::Err<Rich<'tokens, Token, Span>>, _, _>(
        |postfix: Recursive<dyn Parser<'_, I, Spanned<Expr>, extra::Full<Rich<'tokens, Token>, (), ()>>>| {
            choice((
                primary_expression(),
                // array access
                postfix.clone().then(expression().delimited_by(left_bracket(), right_bracket())).map(
                    |((array, array_span), (index, _index_span))| {
                        (Expr::ArrayAccess { array: Box::new(array), index: Box::new(index) }, array_span)
                    },
                ),
                // function call w/o arguments
                postfix.clone().then_ignore(left_paren()).then_ignore(right_paren()).map(
                    |(function, function_span)| {
                        (Expr::FunctionCall { function: Box::new(function), args: Vec::new() }, function_span)
                    },
                ),
                // function call w/ arguments
                postfix.clone().then(argument_expression_list().delimited_by(left_paren(), right_paren())).map(
                    |((function, function_span), args)| {
                        (
                            Expr::FunctionCall {
                                function: Box::new(function),
                                args: args.into_iter().map(|(expr, _span)| expr).collect(), /* TODO this might need
                                                                                             * to change to boxed? */
                            },
                            function_span,
                        )
                    },
                ),
                // member access
                postfix.clone().then_ignore(dot()).then(identifier()).map(
                    |((identifier, identifier_span), (member, _member_span))| {
                        (
                            Expr::MemberAccess { object: Box::new(identifier), member: extract_identifier!(member) },
                            identifier_span,
                        )
                    },
                ),
                // pointer member operation
                postfix.clone().then_ignore(ptr_op()).then(identifier()).map(
                    |((identifier, identifier_span), (member, _member_span))| {
                        (
                            Expr::MemberAccess { object: Box::new(identifier), member: extract_identifier!(member) },
                            identifier_span,
                        )
                    },
                ),
                // increment operation
                postfix.clone().then_ignore(inc()).map(|(identifier, identifier_span)| {
                    (Expr::PostfixOp { op: PostfixOperator::Increment, operand: Box::new(identifier) }, identifier_span)
                }),
                // decrement operation
                postfix.clone().then_ignore(dec()).map(|(identifier, identifier_span)| {
                    (Expr::PostfixOp { op: PostfixOperator::Decrement, operand: Box::new(identifier) }, identifier_span)
                }),
                // compound literals
                left_paren()
                    .ignore_then(type_name())
                    .then_ignore(right_paren().then(left_brace()))
                    .then(initializer_list(initializer())) // TODO this might be some weird recursion problem
                    .then_ignore(comma().or_not())
                    .then_ignore(right_brace())
                    .map(|((type_name, type_name_span), initializers)| {
                        (
                            Expr::CompoundLiteral {
                                type_name: Box::new(type_name),
                                initializers: initializers
                                    .into_iter()
                                    .map(|(initializer, _span)| initializer)
                                    .collect(),
                            },
                            type_name_span,
                        )
                    }),
            ))
        },
    )
}

fn argument_expression_list<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Vec<Spanned<Expr>>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    assignment_expression().separated_by(comma()).at_least(1).collect()
}

fn unary_expression<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    recursive::<_, _, extra::Err<Rich<'tokens, Token, Span>>, _, _>(|unary| {
        choice((
            postfix_expression(),
            // preincrement
            inc().ignore_then(unary.clone()).map(|(unary, unary_span)| {
                (Expr::UnaryOp { op: UnaryOperator::PreIncrement, operand: Box::new(unary) }, unary_span)
            }),
            // predecrement
            dec().ignore_then(unary.clone()).map(|(unary, unary_span)| {
                (Expr::UnaryOp { op: UnaryOperator::PreDecrement, operand: Box::new(unary) }, unary_span)
            }),
            // cast expression
            unary_operator().then(cast_expression()).map(|((unary_op, unary_op_span), (cast, _cast_span))| {
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

fn cast_expression<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    recursive::<_, _, extra::Err<Rich<'tokens, Token, Span>>, _, _>(|cast_expr| {
        choice((
            unary_expression(),
            left_paren().ignore_then(type_name()).then_ignore(right_paren()).then(cast_expr.clone()).map(
                |((type_name, type_name_span), (cast_expr, _cast_expr_span))| {
                    (Expr::Cast { type_name: Box::new(type_name), expr: Box::new(cast_expr) }, type_name_span)
                },
            ),
        ))
    })
}

// TODO left recursion problem
fn multiplicative_expression<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    recursive::<_, _, extra::Err<Rich<'tokens, Token, Span>>, _, _>(|mult| {
        choice((
            cast_expression(),
            mult.clone().then_ignore(star()).then(cast_expression()).map(
                |((mult_expr, mult_span), (cast_expr, _cast_span))| {
                    (
                        Expr::BinaryOp {
                            op: BinaryOperator::Mul,
                            left: Box::new(mult_expr),
                            right: Box::new(cast_expr),
                        },
                        mult_span,
                    )
                },
            ),
            mult.clone().then_ignore(slash()).then(cast_expression()).map(
                |((mult_expr, mult_span), (cast_expr, _cast_span))| {
                    (
                        Expr::BinaryOp {
                            op: BinaryOperator::Div,
                            left: Box::new(mult_expr),
                            right: Box::new(cast_expr),
                        },
                        mult_span,
                    )
                },
            ),
            mult.clone().then_ignore(percent()).then(cast_expression()).map(
                |((mult_expr, mult_span), (cast_expr, _cast_span))| {
                    (
                        Expr::BinaryOp {
                            op: BinaryOperator::Mod,
                            left: Box::new(mult_expr),
                            right: Box::new(cast_expr),
                        },
                        mult_span,
                    )
                },
            ),
        ))
    })
}

// TODO left recursion
fn additive_expression<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    recursive::<_, _, extra::Err<Rich<'tokens, Token, Span>>, _, _>(|additive| {
        choice((
            multiplicative_expression(),
            additive.clone().then_ignore(plus()).then(multiplicative_expression()).map(
                |((add_expr, add_span), (mult_expr, _mult_span))| {
                    (
                        Expr::BinaryOp {
                            op: BinaryOperator::Add,
                            left: Box::new(add_expr),
                            right: Box::new(mult_expr),
                        },
                        add_span,
                    )
                },
            ),
            additive.clone().then_ignore(minus()).then(multiplicative_expression()).map(
                |((add_expr, add_span), (mult_expr, _mult_span))| {
                    (
                        Expr::BinaryOp {
                            op: BinaryOperator::Sub,
                            left: Box::new(add_expr),
                            right: Box::new(mult_expr),
                        },
                        add_span,
                    )
                },
            ),
        ))
    })
}

// TODO left recursion
fn shift_expression<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    recursive::<_, _, extra::Err<Rich<'tokens, Token, Span>>, _, _>(|shift| {
        choice((
            additive_expression(),
            shift.clone().then_ignore(left_op()).then(additive_expression()).map(
                |((shift_expr, shift_span), (add_expr, _add_span))| {
                    (
                        Expr::BinaryOp {
                            op: BinaryOperator::ShiftLeft,
                            left: Box::new(shift_expr),
                            right: Box::new(add_expr),
                        },
                        shift_span,
                    )
                },
            ),
            shift.clone().then_ignore(right_op()).then(additive_expression()).map(
                |((shift_expr, shift_span), (add_expr, _add_span))| {
                    (
                        Expr::BinaryOp {
                            op: BinaryOperator::ShiftRight,
                            left: Box::new(shift_expr),
                            right: Box::new(add_expr),
                        },
                        shift_span,
                    )
                },
            ),
        ))
    })
}

// TODO left recursion
fn relational_expression<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    recursive::<_, _, extra::Err<Rich<'tokens, Token, Span>>, _, _>(|relational| {
        choice((
            shift_expression(),
            relational.clone().then_ignore(lt()).then(shift_expression()).map(
                |((rel_expr, rel_span), (shift_expr, _shift_span))| {
                    (
                        Expr::BinaryOp {
                            op: BinaryOperator::LessThan,
                            left: Box::new(rel_expr),
                            right: Box::new(shift_expr),
                        },
                        rel_span,
                    )
                },
            ),
            relational.clone().then_ignore(gt()).then(shift_expression()).map(
                |((rel_expr, rel_span), (shift_expr, _shift_span))| {
                    (
                        Expr::BinaryOp {
                            op: BinaryOperator::GreaterThan,
                            left: Box::new(rel_expr),
                            right: Box::new(shift_expr),
                        },
                        rel_span,
                    )
                },
            ),
            relational.clone().then_ignore(lte()).then(shift_expression()).map(
                |((rel_expr, rel_span), (shift_expr, _shift_span))| {
                    (
                        Expr::BinaryOp {
                            op: BinaryOperator::LessEqual,
                            left: Box::new(rel_expr),
                            right: Box::new(shift_expr),
                        },
                        rel_span,
                    )
                },
            ),
            relational.clone().then_ignore(gte()).then(shift_expression()).map(
                |((rel_expr, rel_span), (shift_expr, _shift_span))| {
                    (
                        Expr::BinaryOp {
                            op: BinaryOperator::GreaterEqual,
                            left: Box::new(rel_expr),
                            right: Box::new(shift_expr),
                        },
                        rel_span,
                    )
                },
            ),
        ))
    })
}

// TODO left recursion
fn equality_expression<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    recursive::<_, _, extra::Err<Rich<'tokens, Token, Span>>, _, _>(|equality| {
        choice((
            relational_expression(),
            equality.clone().then_ignore(eq()).then(relational_expression()).map(
                |((eq_expr, eq_span), (rel_expr, _rel_span))| {
                    (
                        Expr::BinaryOp {
                            op: BinaryOperator::Equal,
                            left: Box::new(eq_expr),
                            right: Box::new(rel_expr),
                        },
                        eq_span,
                    )
                },
            ),
            equality.clone().then_ignore(ne()).then(relational_expression()).map(
                |((eq_expr, eq_span), (rel_expr, _rel_span))| {
                    (
                        Expr::BinaryOp {
                            op: BinaryOperator::NotEqual,
                            left: Box::new(eq_expr),
                            right: Box::new(rel_expr),
                        },
                        eq_span,
                    )
                },
            ),
        ))
    })
}

// TODO left recursion
fn and_expression<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    recursive::<_, _, extra::Err<Rich<'tokens, Token, Span>>, _, _>(|and| {
        choice((
            equality_expression(),
            and.clone().then_ignore(amp()).then(equality_expression()).map(
                |((and_expr, and_span), (eq_expr, _eq_span))| {
                    (
                        Expr::BinaryOp {
                            op: BinaryOperator::BitwiseAnd,
                            left: Box::new(and_expr),
                            right: Box::new(eq_expr),
                        },
                        and_span,
                    )
                },
            ),
        ))
    })
}

// TODO left recursion, but also not currently used
fn exclusive_or_expression<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    recursive::<_, _, extra::Err<Rich<'tokens, Token, Span>>, _, _>(|or| {
        choice((
            and_expression(),
            or.clone().then_ignore(caret()).then(and_expression()).map(
                |((or_expr, or_span), (and_expr, _and_span))| {
                    (
                        Expr::BinaryOp {
                            op: BinaryOperator::BitwiseXor,
                            left: Box::new(or_expr),
                            right: Box::new(and_expr),
                        },
                        or_span,
                    )
                },
            ),
        ))
    })
}

// TODO left recursion
fn inclusive_or_expression<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    recursive::<_, _, extra::Err<Rich<'tokens, Token, Span>>, _, _>(|or| {
        choice((
            and_expression(),
            or.clone().then_ignore(pipe()).then(and_expression()).map(|((or_expr, or_span), (and_expr, _and_span))| {
                (
                    Expr::BinaryOp {
                        op: BinaryOperator::BitwiseOr,
                        left: Box::new(or_expr),
                        right: Box::new(and_expr),
                    },
                    or_span,
                )
            }),
        ))
    })
}

// TODO left recursion
fn logical_and_expression<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    recursive::<_, _, extra::Err<Rich<'tokens, Token, Span>>, _, _>(|and| {
        choice((
            inclusive_or_expression(),
            and.clone().then_ignore(and_op()).then(inclusive_or_expression()).map(
                |((and_expr, and_span), (or_expr, _or_span))| {
                    (
                        Expr::BinaryOp {
                            op: BinaryOperator::LogicalAnd,
                            left: Box::new(and_expr),
                            right: Box::new(or_expr),
                        },
                        and_span,
                    )
                },
            ),
        ))
    })
}

// TODO left recursion
fn logical_or_expression<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    recursive::<_, _, extra::Err<Rich<'tokens, Token, Span>>, _, _>(|or| {
        choice((
            logical_and_expression(),
            or.clone().then_ignore(or_op()).then(logical_and_expression()).map(
                |((or_expr, or_span), (and_expr, _and_span))| {
                    (
                        Expr::BinaryOp {
                            op: BinaryOperator::LogicalOr,
                            left: Box::new(or_expr),
                            right: Box::new(and_expr),
                        },
                        or_span,
                    )
                },
            ),
        ))
    })
}

fn conditional_expression<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    recursive::<_, _, extra::Err<Rich<'tokens, Token, Span>>, _, _>(|cond| {
        choice((
            logical_or_expression(),
            logical_or_expression()
                .then_ignore(question())
                .then(expression())
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

fn assignment_expression<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    recursive::<_, _, extra::Err<Rich<'tokens, Token, Span>>, _, _>(|assign_expr| {
        choice((
            conditional_expression(),
            unary_expression().then(assignment_operator()).then(assign_expr.clone()).map(
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

fn expression<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    assignment_expression().separated_by(comma()).at_least(1).collect().map(|list: Vec<Spanned<Expr>>| {
        (Expr::Sequence(list.clone().into_iter().map(|(expr, _span)| Box::new(expr)).collect()), list.clone()[0].1)
    })
}

fn constant_expression<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    conditional_expression()
}

fn declaration<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<Declaration>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    choice((
        declaration_specifiers().then_ignore(semicolon()).map(|decl_specifier_list| {
            (
                Declaration {
                    specifiers: decl_specifier_list.clone().into_iter().map(|(specifier, _span)| specifier).collect(),
                    declarators: Vec::new(),
                },
                decl_specifier_list[0].1,
            )
        }),
        declaration_specifiers().then(init_declarator_list()).then_ignore(semicolon()).map(
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

fn declaration_specifiers<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Vec<Spanned<DeclarationSpecifier>>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    let mapped_storage_class_specifier = storage_class_specifier()
        .map(|(storage_class, span)| (DeclarationSpecifier::StorageClass(storage_class), span));
    let mapped_type_qualifier =
        type_qualifier().map(|(qualifier, span)| (DeclarationSpecifier::TypeQualifier(qualifier), span));
    let mapped_type_specifier =
        type_specifier().map(|(specifier, span)| (DeclarationSpecifier::TypeSpecifier(specifier), span));
    let mapped_function_specifier =
        function_specifier().map(|(specifier, span)| (DeclarationSpecifier::FunctionSpecifier(specifier), span));

    mapped_storage_class_specifier
        .or(mapped_type_qualifier)
        .or(mapped_type_specifier)
        .or(mapped_function_specifier)
        .separated_by(comma())
        .collect()
}

fn init_declarator_list<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Vec<Spanned<InitDeclarator>>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    init_declarator().separated_by(comma()).at_least(1).collect()
}

fn init_declarator<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<InitDeclarator>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    declarator().map(|(decl, span)| (InitDeclarator { declarator: decl, initializer: Option::None }, span)).or(
        declarator().then_ignore(eq()).then(initializer()).map(|((decl, decl_span), (init, _init_span))| {
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

fn type_specifier<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<TypeSpecifier>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
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
        // TODO implement later
        // atomic_type_specifier(),
        struct_or_union_specifier(),
        enum_specifier(),
    ))
}

fn struct_or_union_specifier<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<TypeSpecifier>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    // TODO try to understand why boxed is needed here
    choice((
        struct_or_union()
            .then_ignore(left_brace())
            .then(struct_declaration_list())
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
            .then(struct_declaration_list())
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

fn struct_declaration_list<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Vec<Spanned<StructDeclaration>>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    struct_declaration().separated_by(comma()).at_least(1).collect()
}

fn struct_declaration<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<StructDeclaration>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    choice((
        specifier_qualifier_list().then_ignore(semicolon()).map(|specifiers| {
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
        specifier_qualifier_list().then(struct_declarator_list()).then_ignore(semicolon()).map(
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
        // static_assert_declaration(), TODO maybe add this someday
    ))
}

fn specifier_qualifier_list<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Vec<Spanned<TypeQualOrSpec>>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    type_specifier()
        .map(|(specifier, span)| (TypeQualOrSpec::Specifier(specifier), span))
        .or(type_qualifier().map(|(qualifier, span)| (TypeQualOrSpec::Qualifier(qualifier), span)))
        .separated_by(comma())
        .at_least(1)
        .collect()
}

fn struct_declarator_list<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Vec<Spanned<StructDeclarator>>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    struct_declarator().separated_by(comma()).at_least(1).collect()
}

fn struct_declarator<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<StructDeclarator>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    declarator().map(|(declarator, declarator_span)| {
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

fn enum_specifier<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<TypeSpecifier>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    choice((
        // TODO enum_token span should be returned
        enum_token().ignore_then(left_brace()).ignore_then(enumerator_list()).then_ignore(right_brace()).map(
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
            .then(enumerator_list())
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

fn enumerator_list<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Vec<Spanned<Enumerator>>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    enumerator().separated_by(comma()).at_least(1).allow_trailing().collect()
}

fn enumerator<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<Enumerator>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    choice((
        enumeration_constant().then_ignore(eq()).then(constant_expression()).map(
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

fn declarator<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<Declarator>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    choice((
        pointer().then(direct_declarator()).map(|((pointer, pointer_span), (declarator, _declarator_span))| {
            (Declarator { pointer: Some(pointer), direct_declarator: declarator }, pointer_span)
        }),
        direct_declarator().map(|(declarator, declarator_span)| {
            (Declarator { pointer: Option::None, direct_declarator: declarator }, declarator_span)
        }),
    ))
}

fn direct_declarator<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<DirectDeclarator>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    // TODO reformat this using or_not() parser
    recursive::<_, _, extra::Err<Rich<'tokens, Token, Span>>, _, _>(|direct| {
        choice((
            identifier()
                .map(|(identifier, span)| (DirectDeclarator::Identifier(extract_identifier!(identifier)), span)),
            declarator()
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
                // since static token can come before or after type qualifiers
                .then(choice((
                    static_token().ignore_then(type_qualifier_list()),
                    type_qualifier_list().then_ignore(static_token()),
                )))
                .then(assignment_expression())
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
                .then(assignment_expression())
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
                .then(assignment_expression())
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
            direct.clone().then_ignore(left_bracket()).then(assignment_expression()).then_ignore(right_bracket()).map(
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
            direct.clone().then_ignore(left_paren()).then(parameter_type_list()).then_ignore(right_paren()).map(
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

fn parameter_type_list<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<ParameterList>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    parameter_list().then(comma().then(ellipsis()).or_not()).map_with(|(parameters, variadic), e| {
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

fn parameter_list<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Vec<Spanned<ParameterDeclaration>>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    parameter_declaration().separated_by(comma()).at_least(1).collect()
}

fn parameter_declaration<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<ParameterDeclaration>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    choice((
        declaration_specifiers().then(declarator()).map_with(|(specifiers, (declarator, _)), e| {
            (
                ParameterDeclaration {
                    specifiers: specifiers.into_iter().map(|(spec, _)| spec).collect(),
                    declarator: Some(DeclaratorOrAbstract::Declarator(declarator)),
                },
                e.span(),
            )
        }),
        declaration_specifiers().then(abstract_declarator()).map_with(|(specifiers, (abs_declarator, _)), e| {
            (
                ParameterDeclaration {
                    specifiers: specifiers.into_iter().map(|(spec, _)| spec).collect(),
                    declarator: Some(DeclaratorOrAbstract::Abstract(abs_declarator)),
                },
                e.span(),
            )
        }),
        declaration_specifiers().map_with(|specifiers, e| {
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

fn type_name<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<TypeName>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    specifier_qualifier_list().then(abstract_declarator().or_not()).map_with(
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

fn abstract_declarator<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<AbstractDeclarator>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    pointer().or_not().then(direct_abstract_declarator().or_not()).map_with(|(pointer_option, declarator_option), e| {
        (
            AbstractDeclarator {
                pointer: pointer_option.map(|pointer| pointer.0),
                direct: declarator_option.map(|declarator| declarator.0),
            },
            e.span(),
        )
    })
}

// TODO left recursion
fn direct_abstract_declarator<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<DirectAbstractDeclarator>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    recursive::<_, _, extra::Err<Rich<'tokens, Token, Span>>, _, _>(
        |direct_abstract_declarator: Recursive<
            dyn Parser<'_, I, Spanned<DirectAbstractDeclarator>, extra::Full<Rich<'tokens, Token>, (), ()>>,
        >| {
            choice((
                // parenthesized declarator
                left_paren().ignore_then(abstract_declarator()).then_ignore(right_paren()).map_with(
                    |(declarator, _span), e| (DirectAbstractDeclarator::Declarator(Box::new(declarator)), e.span()),
                ),
                // empty array or variable length array
                direct_abstract_declarator
                    .clone()
                    .or_not()
                    .then_ignore(left_bracket())
                    .then(star().or_not())
                    .then_ignore(right_bracket())
                    .map_with(|(direct_option, star_option), e| {
                        (
                            match star_option {
                                Some(_) => DirectAbstractDeclarator::Array {
                                    declarator: direct_option.map(|direct| Box::new(direct.0)),
                                    array_type: ArrayDeclaratorType::VariableLength { qualifiers: Vec::new() },
                                },
                                None => DirectAbstractDeclarator::Array {
                                    declarator: direct_option.map(|direct| Box::new(direct.0)),
                                    array_type: ArrayDeclaratorType::Empty,
                                },
                            },
                            e.span(),
                        )
                    }),
                // static array
                direct_abstract_declarator
                    .clone()
                    .or_not()
                    .then_ignore(left_bracket())
                    .then(choice((
                        static_token().ignore_then(type_qualifier_list().or_not()),
                        type_qualifier_list().or_not().then_ignore(static_token()),
                    )))
                    .then(assignment_expression())
                    .then_ignore(right_bracket())
                    .map_with(|((direct_option, qualifiers_option), (expr, _expr_span)), e| {
                        (
                            DirectAbstractDeclarator::Array {
                                declarator: direct_option.map(|direct| Box::new(direct.0)),
                                array_type: ArrayDeclaratorType::Static {
                                    qualifiers: match qualifiers_option {
                                        Some(qualifiers) => {
                                            qualifiers.clone().into_iter().map(|(qualifier, _span)| qualifier).collect()
                                        }
                                        _ => Vec::new(),
                                    },
                                    size: Box::new(expr),
                                },
                            },
                            e.span(),
                        )
                    }),
            ))
        },
    )
}

fn initializer<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<Initializer>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    recursive(|initializer| {
        choice((
            initializer_list(initializer.clone()).map_with(|list, e| {
                (Initializer::List(list.clone().into_iter().map(|(item, _span)| item).collect()), e.span())
            }),
            assignment_expression().map_with(|(expr, _expr_span), e| (Initializer::Expr(expr), e.span())),
        ))
    })
}

fn initializer_list<'tokens, 'src: 'tokens, I>(
    initializer: impl Parser<'tokens, I, Spanned<Initializer>, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
) -> impl Parser<'tokens, I, Vec<Spanned<InitializerListItem>>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    designation().or_not().then(initializer).separated_by(comma()).allow_trailing().at_least(1).collect().map(
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

fn designation<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<Designation>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    designator_list().then_ignore(eq()).map_with(|list, e| {
        (
            Designation { designators: list.clone().into_iter().map(|(designator, _span)| designator).collect() },
            e.span(),
        )
    })
}

fn designator_list<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Vec<Spanned<Designator>>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    designator().repeated().at_least(1).collect()
}

fn designator<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<Designator>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    choice((
        constant_expression()
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

fn statement<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<Stmt>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    recursive(|statement| {
        choice((
            labeled_statement(statement.clone()),
            compound_statement(),
            expression_statement(),
            selection_statement(statement.clone()),
            iteration_statement(statement.clone()),
            jump_statement(),
        ))
    })
}

fn labeled_statement<'tokens, 'src: 'tokens, I>(
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
        case_token().ignore_then(constant_expression()).then_ignore(colon()).then(statement.clone()).map_with(
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

fn compound_statement<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<Stmt>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    block_item_list().or_not().delimited_by(left_brace(), right_brace()).map_with(|list_option, e| {
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

fn block_item_list<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Vec<Spanned<BlockItem>>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    block_item().repeated().at_least(1).collect()
}

fn block_item<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<BlockItem>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    choice((
        declaration().map_with(|(declaration, _declaration_span), e| (BlockItem::Decl(declaration), e.span())),
        statement().map_with(|(statement, _statement_span), e| (BlockItem::Stmt(statement), e.span())),
    ))
}

fn expression_statement<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<Stmt>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    expression()
        .or_not()
        .then_ignore(semicolon())
        .map_with(|expr_option, e| (Stmt::Expr(expr_option.map(|expr| expr.0)), e.span()))
}

fn selection_statement<'tokens, 'src: 'tokens, I>(
    statement: impl Parser<'tokens, I, Spanned<Stmt>, extra::Err<Rich<'tokens, Token, Span>>> + Clone,
) -> impl Parser<'tokens, I, Spanned<Stmt>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    choice((
        if_token()
            .ignore_then(expression().delimited_by(left_paren(), right_paren()))
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
            .ignore_then(expression().delimited_by(left_paren(), right_paren()))
            .then(statement.clone())
            .map_with(|((expr, _expr_span), (stmt, _stmt_span)), e| {
                (Stmt::Switch { condition: expr, body: Box::new(stmt) }, e.span())
            }),
    ))
}

fn iteration_statement<'tokens, 'src: 'tokens, I>(
    statement: impl Parser<'tokens, I, Spanned<Stmt>, extra::Err<Rich<'tokens, Token, Span>>> + Clone,
) -> impl Parser<'tokens, I, Spanned<Stmt>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    choice((
        while_token()
            .ignore_then(expression().delimited_by(left_paren(), right_paren()))
            .then(statement.clone())
            .map_with(|((expr, _expr_span), (stmt, _stmt_span)), e| {
                (Stmt::While { condition: expr, body: Box::new(stmt) }, e.span())
            }),
        do_token()
            .ignore_then(statement.clone())
            .then_ignore(while_token())
            .then(expression().delimited_by(left_paren(), right_paren()))
            .then_ignore(semicolon())
            .map_with(|((stmt, _stmt_span), (expr, _expr_span)), e| {
                (Stmt::DoWhile { body: Box::new(stmt), condition: expr }, e.span())
            }),
        for_token()
            .ignore_then(expression_statement().then(expression_statement()).then(expression().or_not()))
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
            .ignore_then(declaration().then(expression_statement()).then(expression().or_not()))
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

fn jump_statement<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<Stmt>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
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
            .ignore_then(expression().or_not())
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
    external_declaration().repeated().at_least(1).collect()
}

fn external_declaration<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<ExternalDecl>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    choice((
        function_definition().map_with(|(definition, _span), e| (ExternalDecl::FuncDef(definition), e.span())),
        declaration().map_with(|(declaration, _span), e| (ExternalDecl::Decl(declaration), e.span())),
    ))
}

fn function_definition<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<FunctionDefinition>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    declaration_specifiers().then(declarator()).then(declaration_list().or_not()).then(compound_statement()).map_with(
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

fn declaration_list<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Vec<Spanned<Declaration>>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    declaration().repeated().at_least(1).collect()
}

fn span_from_extra(extra: Extras) -> Span {
    Span::new((), extra.column..extra.column + extra.lexeme.len())
}
