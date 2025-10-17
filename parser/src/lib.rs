use chumsky::{input::ValueInput, prelude::*};
use lexer::*;

pub type Span = SimpleSpan;
pub type Spanned<T> = (T, Span);

#[derive(Clone, Debug)]
pub enum Expr {
    // Literals - values known at compile time
    IntLiteral(i32),
    FloatLiteral(f32),
    StringLiteral(String),
    CharLiteral(char),
    
    // Variable access - needs symbol lookup
    Identifier(String),
    
    // Binary/unary operations
    BinaryOp {
        op: Token,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    UnaryOp {
        op: Token,
        operand: Box<Expr>,
    },
    
    // Other expression types
    FunctionCall {
        function: Box<Expr>,
        args: Vec<Expr>,
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
        type_name: Token,
        expr: Box<Expr>,
    },
    SizeofType(Token),
    SizeofExpr(Box<Expr>),
    TernaryOp {
        condition: Box<Expr>,
        then_expr: Box<Expr>,
        else_expr: Box<Expr>,
    },
    Assignment {
        op: Token,
        lvalue: Box<Expr>,
        rvalue: Box<Expr>,
    },
    // Comma operator
    Sequence(Vec<Expr>),
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
token_parser!(dot, Token::Dot);
token_parser!(amp, Token::Amp);
token_parser!(star, Token::Star);
token_parser!(plus, Token::Plus);
token_parser!(minus, Token::Minus);
token_parser!(tilde, Token::Tilde);
token_parser!(bang, Token::Bang);
token_parser!(slash, Token::Slash);
token_parser!(percent, Token::Percent);
token_parser!(lt, Token::Lt);
token_parser!(gt, Token::Gt);
token_parser!(caret, Token::Caret);
token_parser!(pipe, Token::Pipe);
token_parser!(question, Token::Question);
token_parser!(colon, Token::Colon);
token_parser!(semicolon, Token::Semicolon);
token_parser!(comma, Token::Comma);
token_parser!(assign, Token::Assign);

fn identifier<'tokens, I>() -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<chumsky::error::Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    select! {
        Token::Identifier(extra) => (Expr::Identifier(extra.lexeme.clone()), span_from_extra(extra))
    }
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
            // Parse as i64 to handle larger constants, or use i32 if that's your target
            let value = extra.lexeme.parse::<i32>().unwrap();
            (Expr::IntLiteral(value), span_from_extra(extra))
        },
        Token::FloatConstant(extra) => {
            // Use f64 for better precision, or f32 if that's your target
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

fn postfix_expression<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  recursive::<_, _, extra::Err<Rich<'tokens, Token, Span>>, _, _>(|postfix| {
    let postfix = postfix.boxed();

    primary_expression()
      .or(postfix.then(expression().delimited_by(left_bracket(), right_bracket())).map(|(a, b)| (Expr::ArrayAccess{identifier: a, index: b}, a.1)))
      .or(postfix.then(left_paren())).then_ignore(right_paren())
      .or(postfix.then(argument_expression_list().delimited_by(left_paren(), right_paren())))
      .or(postfix.then(dot().ignored()).then(identifier_string())).map(|a, b| ());
    }
  )
}

fn argument_expression_list<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{

}

fn unary_expression<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{

}

fn unary_operator<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{

}

fn cast_expression<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{

}

fn multiplicative_expression<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{

}

fn additive_expression<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{

}

fn shift_expression<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{

}

fn relational_expression<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{

}

fn equality_expression<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{

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
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn type_name<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
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
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
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