use std::{borrow::Cow, num::IntErrorKind};

use logos::{Lexer, Logos};

use crate::error::ParseErrorKind;

#[derive(Default)]
pub(crate) struct TokenExtras {
    pub errors: Vec<ParseErrorKind>,
}

#[derive(Logos, Debug, PartialEq, Eq)]
#[logos(subpattern exponent = r"[eE][+\-]?[0-9]+")]
#[logos(extras = TokenExtras)]
#[logos(skip r"[\t\v\f\r ]+")]
pub(crate) enum Token<'a> {
    // 标识符
    #[regex("[A-Za-z_][A-Za-z0-9_]*")]
    Identifier(&'a str),
    #[regex(r#"'|""#, |lex| string(lex))]
    StringLiteral(Cow<'a, str>),
    // 关键字
    #[token("struct")]
    Struct,
    #[token("enum")]
    Enum,
    #[token("service")]
    Service,
    #[token("interface")]
    Interface,
    #[token("callback")]
    Callback,
    // 类型关键字
    #[token("u8")]
    U8,
    #[token("i8")]
    I8,
    #[token("u16")]
    U16,
    #[token("i16")]
    I16,
    #[token("u32")]
    U32,
    #[token("i32")]
    I32,
    #[token("u64")]
    U64,
    #[token("i64")]
    I64,
    #[token("f32")]
    F32,
    #[token("f64")]
    F64,
    #[token("bool")]
    Bool,
    #[token("str")]
    Str,
    #[token("void")]
    Void,

    // 常量类型
    #[regex("0", |_| 0)]
    #[regex("0[0-7]+", |lex| int(lex, 8, 1))]
    #[regex("[1-9][0-9]*", |lex| int(lex, 10, 0))]
    #[regex("0[xX][0-9A-Fa-f]+", |lex| int(lex, 16, 2))]
    #[regex("0[oO][0-9A-Fa-f]+", |lex| int(lex, 8, 2))]
    #[regex("0[bB][0-9A-Fa-f]+", |lex| int(lex, 2, 2))]
    Integer(i128),
    #[regex("[0-9]+\\.[0-9]+")] // 匹配浮点数
    Float,
    #[regex("\"[^\"]*\"")] // 匹配字符串常量
    String,

    // 符号
    #[token(":")]
    Colon,
    #[token(";")]
    Semicolon,
    #[token(",")]
    Comma,
    #[token(".")]
    Dot,
    #[token("::")]
    DoubleColon,
    #[token("=")]
    Equals,
    #[token("{")]
    LeftBrace,
    #[token("}")]
    RightBrace,
    #[token("(")]
    LeftParen,
    #[token(")")]
    RightParen,
    #[token("[")]
    LeftBracket,
    #[token("]")]
    RightBracket,
    #[token("->")]
    Arrow,

    // 注释
    #[regex(r#"///[^\n]*\n?"#, line_comment)]
    DocLineComment(Cow<'a, str>),
    #[regex(r#"//[^/][^\n]*\n?"#, line_comment)]
    SingleLineComment(Cow<'a, str>),
    #[token(r#"/*"#, block_comment)]
    BlockComment(Cow<'a, str>),
    #[token("\n")]
    NewLine,
}

fn string<'a>(lex: &mut Lexer<'a, Token<'a>>) -> Cow<'a, str> {
    let mut end = lex.span().end + 1;
    while end < lex.source().len() && lex.source().as_bytes()[end] != b'"' {
        if lex.source().as_bytes()[end] == b'\\' {
            end += 1;
        }
        end += 1;
    }
    if end == lex.source().len() {
        lex.extras.errors.push(ParseErrorKind::UnterminatedString {
            span: lex.span().start..end,
        });
    }
    lex.bump(end + 1 - lex.span().end);
    Cow::Borrowed(&lex.source()[lex.span().start + 1..end])
}

fn int<'a>(lex: &mut Lexer<'a, Token<'a>>, radix: u32, prefix_len: usize) -> Result<i128, ()> {
    debug_assert!(lex.slice().len() > prefix_len);
    let span = lex.span().start + prefix_len..lex.span().end;

    if matches!(lex.remainder().chars().next(), Some(ch) if ch.is_ascii_alphabetic() || ch == '_') {
        let mut end = span.end + 1;
        while end < lex.source().len() && lex.source().as_bytes()[end].is_ascii_alphabetic() {
            end += 1;
        }
        lex.extras
            .errors
            .push(ParseErrorKind::NoSpaceBetweenIntAndIdent {
                span: span.start..end,
            })
    }

    match i128::from_str_radix(&lex.source()[span.clone()], radix) {
        Ok(value) => Ok(value),
        Err(err) => {
            debug_assert_eq!(err.kind(), &IntErrorKind::PosOverflow);
            lex.extras
                .errors
                .push(ParseErrorKind::IntegerOutOfRange { span });
            Ok(Default::default())
        }
    }
}

fn block_comment<'a>(lex: &mut Lexer<'a, Token<'a>>) -> Cow<'a, str> {
    #[derive(Logos)]
    enum Component {
        #[regex(r#"\*/[\t\v\f\r ]*"#)]
        EndComment,
        #[token("/*")]
        StartComment,
        #[token("\n")]
        Newline,
    }

    let mut comment_lexer = Component::lexer(lex.remainder());
    let mut result: Option<Cow<'a, str>> = None;

    let mut depth = 1u32;
    let mut last_end = None;
    let len = loop {
        match comment_lexer.next() {
            Some(Ok(Component::EndComment)) => {
                depth -= 1;
                if depth == 0 {
                    break comment_lexer.span().end;
                } else {
                    last_end = Some(comment_lexer.span().end);
                }
            }
            Some(Ok(Component::StartComment)) => {
                let start = lex.span().end + comment_lexer.span().start;
                let end = lex.span().end + comment_lexer.span().end;
                lex.extras
                    .errors
                    .push(ParseErrorKind::NestedBlockComment { span: start..end });
                depth += 1;
            }
            Some(Ok(Component::Newline)) => {
                cow_push_str(&mut result, "\n");
                let stripped = comment_lexer.remainder().trim_start();
                comment_lexer.bump(comment_lexer.remainder().len() - stripped.len());
                if stripped.starts_with('*') && !stripped.starts_with("*/") {
                    comment_lexer.bump(1);
                }
            }
            Some(Err(())) => cow_push_str(&mut result, comment_lexer.slice()),
            None => {
                if let Some(last_end) = last_end {
                    // This must be a nested block comment
                    break last_end;
                } else {
                    lex.extras.errors.push(ParseErrorKind::UnexpectedEof {
                        expected: "comment terminator".to_owned(),
                    });
                    break lex.remainder().len();
                }
            }
        }
    };

    lex.bump(len);
    normalize_newlines(result.unwrap_or_default())
}

fn cow_push_str<'a>(cow: &mut Option<Cow<'a, str>>, s: &'a str) {
    match cow {
        Some(cow) => cow.to_mut().push_str(s),
        None => *cow = Some(Cow::Borrowed(s)),
    }
}

fn cow_push_bytes<'a>(cow: &mut Option<Cow<'a, [u8]>>, s: &'a [u8]) {
    match cow {
        Some(cow) => cow.to_mut().extend_from_slice(s),
        None => *cow = Some(Cow::Borrowed(s)),
    }
}

fn normalize_newlines(s: Cow<str>) -> Cow<str> {
    if s.contains("\r\n") {
        Cow::Owned(s.replace("\r\n", "\n"))
    } else {
        s
    }
}

fn line_comment<'a>(lex: &mut Lexer<'a, Token<'a>>) -> Cow<'a, str> {
    let content = lex
        .slice()
        .strip_prefix("///")
        .or_else(|| lex.slice().strip_prefix("//"))
        .expect("invalid line comment");
    normalize_newlines(content.into())
}
#[cfg(test)]
mod tests {
    use super::*;
    use logos::Logos;

    #[test]
    fn test_keywords() {
        let input = "struct enum service interface callback";
        let mut lexer = Token::lexer(input);

        assert_eq!(lexer.next(), Some(Ok(Token::Struct)));
        assert_eq!(lexer.slice(), "struct");

        assert_eq!(lexer.next(), Some(Ok(Token::Enum)));
        assert_eq!(lexer.slice(), "enum");

        assert_eq!(lexer.next(), Some(Ok(Token::Service)));
        assert_eq!(lexer.slice(), "service");

        assert_eq!(lexer.next(), Some(Ok(Token::Interface)));
        assert_eq!(lexer.slice(), "interface");

        assert_eq!(lexer.next(), Some(Ok(Token::Callback)));
        assert_eq!(lexer.slice(), "callback");

        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn test_type_keywords() {
        let input = "u8 i8 u16 i16 u32 i32 u64 i64 f32 f64 bool str void";
        let mut lexer = Token::lexer(input);

        assert_eq!(lexer.next(), Some(Ok(Token::U8)));
        assert_eq!(lexer.slice(), "u8");

        assert_eq!(lexer.next(), Some(Ok(Token::I8)));
        assert_eq!(lexer.slice(), "i8");

        assert_eq!(lexer.next(), Some(Ok(Token::U16)));
        assert_eq!(lexer.slice(), "u16");

        assert_eq!(lexer.next(), Some(Ok(Token::I16)));
        assert_eq!(lexer.slice(), "i16");

        assert_eq!(lexer.next(), Some(Ok(Token::U32)));
        assert_eq!(lexer.slice(), "u32");

        assert_eq!(lexer.next(), Some(Ok(Token::I32)));
        assert_eq!(lexer.slice(), "i32");

        assert_eq!(lexer.next(), Some(Ok(Token::U64)));
        assert_eq!(lexer.slice(), "u64");

        assert_eq!(lexer.next(), Some(Ok(Token::I64)));
        assert_eq!(lexer.slice(), "i64");

        assert_eq!(lexer.next(), Some(Ok(Token::F32)));
        assert_eq!(lexer.slice(), "f32");

        assert_eq!(lexer.next(), Some(Ok(Token::F64)));
        assert_eq!(lexer.slice(), "f64");

        assert_eq!(lexer.next(), Some(Ok(Token::Bool)));
        assert_eq!(lexer.slice(), "bool");

        assert_eq!(lexer.next(), Some(Ok(Token::Str)));
        assert_eq!(lexer.slice(), "str");

        assert_eq!(lexer.next(), Some(Ok(Token::Void)));
        assert_eq!(lexer.slice(), "void");

        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn test_identifiers() {
        let input = "my_var _anotherVar var123";
        let mut lexer = Token::lexer(input);

        assert_eq!(lexer.next(), Some(Ok(Token::Identifier("my_var"))));
        assert_eq!(lexer.slice(), "my_var");

        assert_eq!(lexer.next(), Some(Ok(Token::Identifier("_anotherVar"))));
        assert_eq!(lexer.slice(), "_anotherVar");

        assert_eq!(lexer.next(), Some(Ok(Token::Identifier("var123"))));
        assert_eq!(lexer.slice(), "var123");

        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn test_literals() {
        let input = "123 0x1f 0b1010 0o77 12.34 \"hello\"";
        let mut lexer = Token::lexer(input);

        assert_eq!(lexer.next(), Some(Ok(Token::Integer(123))));

        assert_eq!(lexer.next(), Some(Ok(Token::Integer(0x1f))));

        assert_eq!(lexer.next(), Some(Ok(Token::Integer(0b1010))));

        assert_eq!(lexer.next(), Some(Ok(Token::Integer(0o77))));

        assert_eq!(lexer.next(), Some(Ok(Token::Float)));
        assert_eq!(lexer.slice(), "12.34");

        assert_eq!(lexer.next(), Some(Ok(Token::String)));
        assert_eq!(lexer.slice(), "\"hello\"");

        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn test_symbols() {
        let input = ": ; , . :: = { } ( ) [ ] ->";
        let mut lexer = Token::lexer(input);

        assert_eq!(lexer.next(), Some(Ok(Token::Colon)));
        assert_eq!(lexer.slice(), ":");

        assert_eq!(lexer.next(), Some(Ok(Token::Semicolon)));
        assert_eq!(lexer.slice(), ";");

        assert_eq!(lexer.next(), Some(Ok(Token::Comma)));
        assert_eq!(lexer.slice(), ",");

        assert_eq!(lexer.next(), Some(Ok(Token::Dot)));
        assert_eq!(lexer.slice(), ".");

        assert_eq!(lexer.next(), Some(Ok(Token::DoubleColon)));
        assert_eq!(lexer.slice(), "::");

        assert_eq!(lexer.next(), Some(Ok(Token::Equals)));
        assert_eq!(lexer.slice(), "=");

        assert_eq!(lexer.next(), Some(Ok(Token::LeftBrace)));
        assert_eq!(lexer.slice(), "{");

        assert_eq!(lexer.next(), Some(Ok(Token::RightBrace)));
        assert_eq!(lexer.slice(), "}");

        assert_eq!(lexer.next(), Some(Ok(Token::LeftParen)));
        assert_eq!(lexer.slice(), "(");

        assert_eq!(lexer.next(), Some(Ok(Token::RightParen)));
        assert_eq!(lexer.slice(), ")");

        assert_eq!(lexer.next(), Some(Ok(Token::LeftBracket)));
        assert_eq!(lexer.slice(), "[");

        assert_eq!(lexer.next(), Some(Ok(Token::RightBracket)));
        assert_eq!(lexer.slice(), "]");

        assert_eq!(lexer.next(), Some(Ok(Token::Arrow)));
        assert_eq!(lexer.slice(), "->");

        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn test_whitespace_and_newline() {
        let input = " \t\n";
        let mut lexer = Token::lexer(input);

        assert_eq!(lexer.next(), Some(Ok(Token::NewLine)));
        assert_eq!(lexer.slice(), "\n");

        assert_eq!(lexer.next(), None);
    }
}
