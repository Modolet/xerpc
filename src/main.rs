use chumsky::Parser;
use logos::Logos;
use parser::root_parser;
use token::Token;
mod error;
mod parser;
mod token;

fn get_tokens(input: &str) -> Vec<Token> {
    Token::lexer(input)
        .spanned()
        .map(|(token, _)| token.unwrap())
        .filter(|x| {
            !matches!(
                x,
                Token::SingleLineComment(_) | Token::NewLine | Token::BlockComment(_)
            )
        })
        .collect::<Vec<_>>()
}

fn main() {
    let input1 = include_str!("../test.xerpc");

    let tokens = get_tokens(input1);
    let ast = root_parser().parse(tokens);
    dbg!(&ast);
}
