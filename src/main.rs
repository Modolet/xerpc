#![allow(dead_code)]

use logos::Logos;
use token::Token;
mod error;
mod token;

const MAX_MESSAGE_FIELD_NUMBER: i32 = 536_870_911;

fn main() {
    let input = include_str!("../test.xerpc");
    let mut lexer = token::Token::lexer(input);
    while let Some(token) = lexer.next() {
        match token {
            Ok(Token::NewLine) => {
                println!();
            }
            Ok(Token::SingleLineComment(_) | Token::BlockComment(_) | Token::DocLineComment(_)) => {
                println!("{:?}", token);
            }
            Ok(token) => {
                print!("{:?} ", token);
            }
            Err(e) => {
                println!("{:?} {:?}", e, lexer.slice());
            }
        }
    }
}
