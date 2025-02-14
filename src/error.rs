use std::{fmt, ops::Range};

use logos::Span;
use miette::{Diagnostic, NamedSource, SourceCode};
use thiserror::Error;

/// An error that may occur while parsing a protobuf source file.
#[derive(Error, Diagnostic)]
#[error("{}", kind)]
#[diagnostic(forward(kind))]
pub struct ParseError {
    kind: Box<ParseErrorKind>,
    #[related]
    related: Vec<ParseErrorKind>,
    #[source_code]
    source_code: NamedSource<String>,
}

#[derive(Error, Debug, Diagnostic, PartialEq)]
pub(crate) enum ParseErrorKind {
    #[error("integer is too large")]
    IntegerOutOfRange {
        #[label("integer defined here")]
        span: Span,
    },
    #[error("invalid float")]
    InvalidFloat {
        #[label("float defined here")]
        span: Span,
    },
    #[error("unterminated string")]
    UnterminatedString {
        #[label("string starts here")]
        span: Span,
    },
    #[error("nested block comments are not supported")]
    NestedBlockComment {
        #[label("defined here")]
        span: Span,
    },
    #[error("whitespace is required between an integer literal and an identifier")]
    NoSpaceBetweenIntAndIdent {
        #[label("found here")]
        span: Span,
    },
    #[error("expected {expected}, but reached end of file")]
    UnexpectedEof { expected: String },
}

impl ParseError {
    pub(crate) fn new(mut related: Vec<ParseErrorKind>, name: &str, source: String) -> Self {
        debug_assert!(!related.is_empty());
        let kind = related.remove(0);
        ParseError {
            kind: Box::new(kind),
            related,
            source_code: NamedSource::new(name, source),
        }
    }

    #[cfg(test)]
    pub(crate) fn into_inner(mut self) -> Vec<ParseErrorKind> {
        self.related.insert(0, *self.kind);
        self.related
    }

    /// Gets the name of the file in which this error occurred.
    pub fn file(&self) -> &str {
        self.source_code.name()
    }

    /// Gets the primary source code span associated with this error, if any.
    pub fn span(&self) -> Option<Range<usize>> {
        match &*self.kind {
            ParseErrorKind::IntegerOutOfRange { span } => Some(span.clone()),
            ParseErrorKind::UnterminatedString { span } => Some(span.clone()),
            ParseErrorKind::NestedBlockComment { span } => Some(span.clone()),
            ParseErrorKind::NoSpaceBetweenIntAndIdent { span } => Some(span.clone()),
            ParseErrorKind::UnexpectedEof { .. } => None,
            ParseErrorKind::InvalidFloat { span } => Some(span.clone()),
        }
    }
}

impl fmt::Debug for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(span) = self.span() {
            if let Ok(span_contents) = self.source_code.read_span(&span.into(), 0, 0) {
                if let Some(file_name) = span_contents.name() {
                    write!(f, "{}:", file_name)?;
                }

                write!(
                    f,
                    "{}:{}: ",
                    span_contents.line() + 1,
                    span_contents.column() + 1
                )?;
            }
        }

        write!(f, "{}", self)
    }
}
