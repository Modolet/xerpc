use std::num::IntErrorKind;

use logos::{Lexer, Logos};

use crate::error::ParseErrorKind;

#[derive(Default)]
pub(crate) struct TokenExtras {
    pub errors: Vec<ParseErrorKind>,
}

#[derive(Logos, Debug, PartialEq, Clone, Hash, Eq)]
#[logos(subpattern exponent = r"[eE][+\-]?[0-9]+")]
#[logos(extras = TokenExtras)]
#[logos(skip r"[\t\v\f\r ]+")]
pub(crate) enum Token {
    // 标识符
    #[regex(r#"[A-Za-z_][A-Za-z0-9_]*"#, |lex| lex.slice().to_string())]
    Identifier(String),
    #[regex(r#"'|""#, |lex| string(lex))]
    StringLiteral(String),
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
    #[token("fn")]
    Fn,
    #[token("const")]
    Const,
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
    #[token("true")]
    True,
    #[token("false")]
    False,

    // 常量类型
    #[regex("0", |_| 0)]
    #[regex("0[0-7]+", |lex| int(lex, 8, 1))]
    #[regex("[1-9][0-9]*", |lex| int(lex, 10, 0))]
    #[regex("0[xX][0-9A-Fa-f]+", |lex| int(lex, 16, 2))]
    #[regex("0[oO][0-9A-Fa-f]+", |lex| int(lex, 8, 2))]
    #[regex("0[bB][0-9A-Fa-f]+", |lex| int(lex, 2, 2))]
    Integer(i128),
    #[regex("[0-9]+\\.[0-9]+", |lex| float(lex))] // 匹配浮点数
    Float(String),
    #[regex("\"[^\"]*\"", |lex| lex.slice().to_string())] // 匹配字符串常量
    String(String),
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
    DocLineComment(String),
    #[regex(r#"//[^/][^\n]*\n?"#, line_comment)]
    SingleLineComment(String),
    #[token(r#"/*"#, block_comment)]
    BlockComment(String),
    #[token("\n")]
    NewLine,
}

fn string(lex: &mut Lexer<Token>) -> String {
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
    lex.source()[lex.span().start + 1..end].to_string()
}

fn int(lex: &mut Lexer<Token>, radix: u32, prefix_len: usize) -> Result<i128, ()> {
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

fn float(lex: &mut Lexer<Token>) -> Result<String, ()> {
    if lex.slice().parse::<f64>().is_err() {
        lex.extras.errors.push(ParseErrorKind::InvalidFloat {
            span: lex.span().start..lex.span().end,
        });
        Err(())
    } else {
        Ok(lex.slice().to_string())
    }
}

fn block_comment(lex: &mut Lexer<Token>) -> String {
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
    let mut result = String::new();

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
                result.push('\n');
                let stripped = comment_lexer.remainder().trim_start();
                comment_lexer.bump(comment_lexer.remainder().len() - stripped.len());
                if stripped.starts_with('*') && !stripped.starts_with("*/") {
                    comment_lexer.bump(1);
                }
            }
            Some(Err(())) => result.push_str(comment_lexer.slice()),
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
    normalize_newlines(&result)
}

fn normalize_newlines(s: &str) -> String {
    s.replace("\r\n", "\n")
}

fn line_comment(lex: &mut Lexer<Token>) -> String {
    let content = lex
        .slice()
        .strip_prefix("///")
        .or_else(|| lex.slice().strip_prefix("//"))
        .expect("invalid line comment");
    normalize_newlines(content)
}
