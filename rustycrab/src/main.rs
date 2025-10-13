use logos::Logos;

use lexer::Token;

fn main() {
    let src = r#"
        int main() {
            // test
            printf("Hello, world!\n");
            return 0;
        }
    "#;

    let mut lex = Token::lexer(src);
    while let Some(tok) = lex.next() {
        println!("{:?}", tok);
    }
}
