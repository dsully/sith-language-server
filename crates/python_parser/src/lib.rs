//! This crate can be used to parse Python source code into an Abstract
//! Syntax Tree.
//!
//! ## Overview
//!
//! The process by which source code is parsed into an AST can be broken down
//! into two general stages: [lexical analysis] and [parsing].
//!
//! During lexical analysis, the source code is converted into a stream of lexical
//! tokens that represent the smallest meaningful units of the language. For example,
//! the source code `print("Hello world")` would _roughly_ be converted into the following
//! stream of tokens:
//!
//! ```text
//! Name("print"), LeftParen, String("Hello world"), RightParen
//! ```
//!
//! These tokens are then consumed by the `python_parser`, which matches them against a set of
//! grammar rules to verify that the source code is syntactically valid and to construct
//! an AST that represents the source code.
//!
//! During parsing, the `python_parser` consumes the tokens generated by the lexer and constructs
//! a tree representation of the source code. The tree is made up of nodes that represent
//! the different syntactic constructs of the language. If the source code is syntactically
//! invalid, parsing fails and an error is returned. After a successful parse, the AST can
//! be used to perform further analysis on the source code. Continuing with the example
//! above, the AST generated by the `python_parser` would _roughly_ look something like this:
//!
//! ```text
//! node: Expr {
//!     value: {
//!         node: Call {
//!             func: {
//!                 node: Name {
//!                     id: "print",
//!                     ctx: Load,
//!                 },
//!             },
//!             args: [
//!                 node: Constant {
//!                     value: Str("Hello World"),
//!                     kind: None,
//!                 },
//!             ],
//!             keywords: [],
//!         },
//!     },
//! },
//!```
//!
//! **Note:** The Tokens/ASTs shown above are not the exact tokens/ASTs generated by the `python_parser`.
//! Refer to the [playground](https://play.ruff.rs) for the correct representation.
//!
//! ## Source code layout
//!
//! The functionality of this crate is split into several modules:
//!
//! - token: This module contains the definition of the tokens that are generated by the lexer.
//! - [lexer]: This module contains the lexer and is responsible for generating the tokens.
//! - parser: This module contains an interface to the [Parsed] and is responsible for generating the AST.
//! - mode: This module contains the definition of the different modes that the `python_parser` can be in.
//!
//! [lexical analysis]: https://en.wikipedia.org/wiki/Lexical_analysis
//! [parsing]: https://en.wikipedia.org/wiki/Parsing
//! [lexer]: crate::lexer

use std::iter::FusedIterator;
use std::ops::Deref;

pub use crate::error::{FStringErrorType, ParseError, ParseErrorType};
pub use crate::token::{Token, TokenKind};

use crate::parser::Parser;

use python_ast::{Expr, Mod, ModExpression, ModModule, PySourceType, Suite};
use ruff_text_size::{Ranged, TextRange, TextSize};

mod error;
pub mod lexer;
mod parser;
mod string;
mod token;
mod token_set;
mod token_source;

/// Parse a full Python module usually consisting of multiple lines.
///
/// This is a convenience function that can be used to parse a full Python program without having to
/// specify the [`Mode`] or the location. It is probably what you want to use most of the time.
///
/// # Example
///
/// For example, parsing a simple function definition and a call to that function:
///
/// ```
/// use python_parser::parse_module;
///
/// let source = r#"
/// def foo():
///    return 42
///
/// print(foo())
/// "#;
///
/// let module = parse_module(source);
/// assert!(module.is_valid());
/// ```
pub fn parse_module(source: &str) -> Parsed<ModModule> {
    Parser::new(source, Mode::Module)
        .parse()
        .try_into_module()
        .unwrap()
}

/// Parses a single Python expression.
///
/// This convenience function can be used to parse a single expression without having to
/// specify the Mode or the location.
///
/// # Example
///
/// For example, parsing a single expression denoting the addition of two numbers:
///
/// ```
/// use python_parser::parse_expression;
///
/// let expr = parse_expression("1 + 2");
/// assert!(expr.is_ok());
/// ```
pub fn parse_expression(source: &str) -> Result<Parsed<ModExpression>, ParseError> {
    Parser::new(source, Mode::Expression)
        .parse()
        .try_into_expression()
        .unwrap()
        .into_result()
}

/// Parses a Python expression for the given range in the source.
///
/// This function allows to specify the range of the expression in the source code, other than
/// that, it behaves exactly like [`parse_expression`].
///
/// # Example
///
/// Parsing one of the numeric literal which is part of an addition expression:
///
/// ```
/// use python_parser::parse_expression_range;
/// # use ruff_text_size::{TextRange, TextSize};
///
/// let parsed = parse_expression_range("11 + 22 + 33", TextRange::new(TextSize::new(5), TextSize::new(7)));
/// assert!(parsed.is_ok());
/// ```
pub fn parse_expression_range(
    source: &str,
    range: TextRange,
) -> Result<Parsed<ModExpression>, ParseError> {
    let source = &source[..range.end().to_usize()];
    Parser::new_starts_at(source, Mode::Expression, range.start())
        .parse()
        .try_into_expression()
        .unwrap()
        .into_result()
}

/// Parse the given Python source code using the specified [`Mode`].
///
/// This function is the most general function to parse Python code. Based on the [`Mode`] supplied,
/// it can be used to parse a single expression, a full Python program, an interactive expression
/// or a Python program containing IPython escape commands.
///
/// # Example
///
/// If we want to parse a simple expression, we can use the [`Mode::Expression`] mode during
/// parsing:
///
/// ```
/// use python_parser::{Mode, parse};
///
/// let parsed = parse("1 + 2", Mode::Expression);
/// assert!(parsed.is_ok());
/// ```
///
/// Alternatively, we can parse a full Python program consisting of multiple lines:
///
/// ```
/// use python_parser::{Mode, parse};
///
/// let source = r#"
/// class Greeter:
///
///   def greet(self):
///    print("Hello, world!")
/// "#;
/// let parsed = parse(source, Mode::Module);
/// assert!(parsed.is_ok());
/// ```
///
/// Additionally, we can parse a Python program containing IPython escapes:
///
/// ```
/// use python_parser::{Mode, parse};
///
/// let source = r#"
/// %timeit 1 + 2
/// ?str.replace
/// !ls
/// "#;
/// let parsed = parse(source, Mode::Ipython);
/// assert!(parsed.is_ok());
/// ```
pub fn parse(source: &str, mode: Mode) -> Result<Parsed<Mod>, ParseError> {
    parse_unchecked(source, mode).into_result()
}

/// Parse the given Python source code using the specified [`Mode`].
///
/// This is same as the [`parse`] function except that it doesn't check for any [`ParseError`]
/// and returns the [`Parsed`] as is.
pub fn parse_unchecked(source: &str, mode: Mode) -> Parsed<Mod> {
    Parser::new(source, mode).parse()
}

/// Parse the given Python source code using the specified [`PySourceType`].
pub fn parse_unchecked_source(source: &str, source_type: PySourceType) -> Parsed<ModModule> {
    // SAFETY: Safe because `PySourceType` always parses to a `ModModule`
    Parser::new(source, source_type.as_mode())
        .parse()
        .try_into_module()
        .unwrap()
}

/// Represents the parsed source code.
#[derive(Debug, PartialEq, Clone, Default)]
pub struct Parsed<T> {
    syntax: T,
    errors: Vec<ParseError>,
}

impl<T> Parsed<T> {
    /// Returns the syntax node represented by this parsed output.
    pub fn syntax(&self) -> &T {
        &self.syntax
    }

    /// Returns a list of syntax errors found during parsing.
    pub fn errors(&self) -> &[ParseError] {
        &self.errors
    }

    /// Consumes the [`Parsed`] output and returns the contained syntax node.
    pub fn into_syntax(self) -> T {
        self.syntax
    }

    /// Consumes the [`Parsed`] output and returns a list of syntax errors found during parsing.
    pub fn into_errors(self) -> Vec<ParseError> {
        self.errors
    }

    /// Returns `true` if the parsed source code is valid i.e., it has no syntax errors.
    pub fn is_valid(&self) -> bool {
        self.errors.is_empty()
    }

    /// Returns the [`Parsed`] output as a [`Result`], returning [`Ok`] if it has no syntax errors,
    /// or [`Err`] containing the first [`ParseError`] encountered.
    pub fn as_result(&self) -> Result<&Parsed<T>, &[ParseError]> {
        if self.is_valid() {
            Ok(self)
        } else {
            Err(&self.errors)
        }
    }

    /// Consumes the [`Parsed`] output and returns a [`Result`] which is [`Ok`] if it has no syntax
    /// errors, or [`Err`] containing the first [`ParseError`] encountered.
    pub(crate) fn into_result(self) -> Result<Parsed<T>, ParseError> {
        if self.is_valid() {
            Ok(self)
        } else {
            Err(self.into_errors().into_iter().next().unwrap())
        }
    }
}

impl Parsed<Mod> {
    /// Attempts to convert the [`Parsed<Mod>`] into a [`Parsed<ModModule>`].
    ///
    /// This method checks if the `syntax` field of the output is a [`Mod::Module`]. If it is, the
    /// method returns [`Some(Parsed<ModModule>)`] with the contained module. Otherwise, it
    /// returns [`None`].
    ///
    /// [`Some(Parsed<ModModule>)`]: Some
    pub fn try_into_module(self) -> Option<Parsed<ModModule>> {
        match self.syntax {
            Mod::Module(module) => Some(Parsed {
                syntax: module,
                errors: self.errors,
            }),
            Mod::Expression(_) => None,
        }
    }

    /// Attempts to convert the [`Parsed<Mod>`] into a [`Parsed<ModExpression>`].
    ///
    /// This method checks if the `syntax` field of the output is a [`Mod::Expression`]. If it is,
    /// the method returns [`Some(Parsed<ModExpression>)`] with the contained expression.
    /// Otherwise, it returns [`None`].
    ///
    /// [`Some(Parsed<ModExpression>)`]: Some
    pub fn try_into_expression(self) -> Option<Parsed<ModExpression>> {
        match self.syntax {
            Mod::Module(_) => None,
            Mod::Expression(expression) => Some(Parsed {
                syntax: expression,
                errors: self.errors,
            }),
        }
    }
}

impl Parsed<ModModule> {
    /// Returns the module body contained in this parsed output as a [`Suite`].
    pub fn suite(&self) -> &Suite {
        &self.syntax.body
    }

    /// Consumes the [`Parsed`] output and returns the module body as a [`Suite`].
    pub fn into_suite(self) -> Suite {
        self.syntax.body
    }
}

impl Parsed<ModExpression> {
    /// Returns the expression contained in this parsed output.
    pub fn expr(&self) -> &Expr {
        &self.syntax.body
    }

    /// Returns a mutable reference to the expression contained in this parsed output.
    pub fn expr_mut(&mut self) -> &mut Expr {
        &mut self.syntax.body
    }

    /// Consumes the [`Parsed`] output and returns the contained [`Expr`].
    pub fn into_expr(self) -> Expr {
        *self.syntax.body
    }
}

/// Tokens represents a vector of lexed [`Token`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Tokens {
    raw: Vec<Token>,
}

impl Tokens {
    pub(crate) fn new(tokens: Vec<Token>) -> Tokens {
        Tokens { raw: tokens }
    }

    /// Returns an iterator over all the tokens that provides context.
    pub fn iter_with_context(&self) -> TokenIterWithContext {
        TokenIterWithContext::new(&self.raw)
    }

    /// Returns a slice of [`Token`] that are within the given `range`.
    ///
    /// The start and end offset of the given range should be either:
    /// 1. Token boundary
    /// 2. Gap between the tokens
    ///
    /// For example, considering the following tokens and their corresponding range:
    ///
    /// | Token               | Range     |
    /// |---------------------|-----------|
    /// | `Def`               | `0..3`    |
    /// | `Name`              | `4..7`    |
    /// | `Lpar`              | `7..8`    |
    /// | `Rpar`              | `8..9`    |
    /// | `Colon`             | `9..10`   |
    /// | `Newline`           | `10..11`  |
    /// | `Comment`           | `15..24`  |
    /// | `NonLogicalNewline` | `24..25`  |
    /// | `Indent`            | `25..29`  |
    /// | `Pass`              | `29..33`  |
    ///
    /// Here, for (1) a token boundary is considered either the start or end offset of any of the
    /// above tokens. For (2), the gap would be any offset between the `Newline` and `Comment`
    /// token which are 12, 13, and 14.
    ///
    /// Examples:
    /// 1) `4..10` would give `Name`, `Lpar`, `Rpar`, `Colon`
    /// 2) `11..25` would give `Comment`, `NonLogicalNewline`
    /// 3) `12..25` would give same as (2) and offset 12 is in the "gap"
    /// 4) `9..12` would give `Colon`, `Newline` and offset 12 is in the "gap"
    /// 5) `18..27` would panic because both the start and end offset is within a token
    ///
    /// ## Note
    ///
    /// The returned slice can contain the [`TokenKind::Unknown`] token if there was a lexical
    /// error encountered within the given range.
    ///
    /// # Panics
    ///
    /// If either the start or end offset of the given range is within a token range.
    pub fn in_range(&self, range: TextRange) -> &[Token] {
        let tokens_after_start = self.after(range.start());

        match tokens_after_start.binary_search_by_key(&range.end(), Ranged::end) {
            Ok(idx) => {
                // If we found the token with the end offset, that token should be included in the
                // return slice.
                &tokens_after_start[..=idx]
            }
            Err(idx) => {
                if let Some(token) = tokens_after_start.get(idx) {
                    // If it's equal to the start offset, then it's at a token boundary which is
                    // valid. If it's less than the start offset, then it's in the gap between the
                    // tokens which is valid as well.
                    assert!(
                        range.end() <= token.start(),
                        "End offset {:?} is inside a token range {:?}",
                        range.end(),
                        token.range()
                    );
                }

                // This index is where the token with the offset _could_ be, so that token should
                // be excluded from the return slice.
                &tokens_after_start[..idx]
            }
        }
    }

    /// Returns a slice of tokens after the given [`TextSize`] offset.
    ///
    /// If the given offset is between two tokens, the returned slice will start from the following
    /// token. In other words, if the offset is between the end of previous token and start of next
    /// token, the returned slice will start from the next token.
    ///
    /// # Panics
    ///
    /// If the given offset is inside a token range.
    pub fn after(&self, offset: TextSize) -> &[Token] {
        match self.binary_search_by(|token| token.start().cmp(&offset)) {
            Ok(idx) => &self[idx..],
            Err(idx) => {
                // We can't use `saturating_sub` here because a file could contain a BOM header, in
                // which case the token starts at offset 3 for UTF-8 encoded file content.
                if idx > 0 {
                    if let Some(prev) = self.get(idx - 1) {
                        // If it's equal to the end offset, then it's at a token boundary which is
                        // valid. If it's greater than the end offset, then it's in the gap between
                        // the tokens which is valid as well.
                        assert!(
                            offset >= prev.end(),
                            "Offset {:?} is inside a token range {:?}",
                            offset,
                            prev.range()
                        );
                    }
                }

                &self[idx..]
            }
        }
    }
}

impl<'a> IntoIterator for &'a Tokens {
    type Item = &'a Token;
    type IntoIter = std::slice::Iter<'a, Token>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl Deref for Tokens {
    type Target = [Token];

    fn deref(&self) -> &Self::Target {
        &self.raw
    }
}

/// An iterator over the [`Token`]s with context.
///
/// This struct is created by the [`iter_with_context`] method on [`Tokens`]. Refer to its
/// documentation for more details.
///
/// [`iter_with_context`]: Tokens::iter_with_context
#[derive(Debug, Clone)]
pub struct TokenIterWithContext<'a> {
    inner: std::slice::Iter<'a, Token>,
    nesting: u32,
}

impl<'a> TokenIterWithContext<'a> {
    fn new(tokens: &'a [Token]) -> TokenIterWithContext<'a> {
        TokenIterWithContext {
            inner: tokens.iter(),
            nesting: 0,
        }
    }

    /// Return the nesting level the iterator is currently in.
    pub const fn nesting(&self) -> u32 {
        self.nesting
    }

    /// Returns `true` if the iterator is within a parenthesized context.
    pub const fn in_parenthesized_context(&self) -> bool {
        self.nesting > 0
    }

    /// Returns the next [`Token`] in the iterator without consuming it.
    pub fn peek(&self) -> Option<&'a Token> {
        self.clone().next()
    }
}

impl<'a> Iterator for TokenIterWithContext<'a> {
    type Item = &'a Token;

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.inner.next()?;

        match token.kind() {
            TokenKind::Lpar | TokenKind::Lbrace | TokenKind::Lsqb => self.nesting += 1,
            TokenKind::Rpar | TokenKind::Rbrace | TokenKind::Rsqb => {
                self.nesting = self.nesting.saturating_sub(1);
            }
            // This mimics the behavior of re-lexing which reduces the nesting level on the lexer.
            // We don't need to reduce it by 1 because unlike the lexer we see the final token
            // after recovering from every unclosed parenthesis.
            TokenKind::Newline if self.nesting > 0 => {
                self.nesting = 0;
            }
            _ => {}
        }

        Some(token)
    }
}

impl FusedIterator for TokenIterWithContext<'_> {}

/// Control in the different modes by which a source file can be parsed.
///
/// The mode argument specifies in what way code must be parsed.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum Mode {
    /// The code consists of a sequence of statements.
    Module,

    /// The code consists of a single expression.
    Expression,

    /// The code consists of a sequence of statements which can include the
    /// escape commands that are part of IPython syntax.
    ///
    /// ## Supported escape commands:
    ///
    /// - [Magic command system] which is limited to [line magics] and can start
    ///   with `?` or `??`.
    /// - [Dynamic object information] which can start with `?` or `??`.
    /// - [System shell access] which can start with `!` or `!!`.
    /// - [Automatic parentheses and quotes] which can start with `/`, `;`, or `,`.
    ///
    /// [Magic command system]: https://ipython.readthedocs.io/en/stable/interactive/reference.html#magic-command-system
    /// [line magics]: https://ipython.readthedocs.io/en/stable/interactive/magics.html#line-magics
    /// [Dynamic object information]: https://ipython.readthedocs.io/en/stable/interactive/reference.html#dynamic-object-information
    /// [System shell access]: https://ipython.readthedocs.io/en/stable/interactive/reference.html#system-shell-access
    /// [Automatic parentheses and quotes]: https://ipython.readthedocs.io/en/stable/interactive/reference.html#automatic-parentheses-and-quotes
    Ipython,
}

impl std::str::FromStr for Mode {
    type Err = ModeParseError;
    fn from_str(s: &str) -> Result<Self, ModeParseError> {
        match s {
            "exec" | "single" => Ok(Mode::Module),
            "eval" => Ok(Mode::Expression),
            "ipython" => Ok(Mode::Ipython),
            _ => Err(ModeParseError),
        }
    }
}

/// A type that can be represented as [Mode].
pub trait AsMode {
    fn as_mode(&self) -> Mode;
}

impl AsMode for PySourceType {
    fn as_mode(&self) -> Mode {
        match self {
            PySourceType::Python | PySourceType::Stub => Mode::Module,
            PySourceType::Ipynb => Mode::Ipython,
        }
    }
}

/// Returned when a given mode is not valid.
#[derive(Debug)]
pub struct ModeParseError;

impl std::fmt::Display for ModeParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, r#"mode must be "exec", "eval", "ipython", or "single""#)
    }
}

#[cfg(test)]
mod tests {
    use std::ops::Range;

    use crate::token::TokenFlags;

    use super::*;

    /// Test case containing a "gap" between two tokens.
    ///
    /// Code: <https://play.ruff.rs/a3658340-6df8-42c5-be80-178744bf1193>
    const TEST_CASE_WITH_GAP: [(TokenKind, Range<u32>); 10] = [
        (TokenKind::Def, 0..3),
        (TokenKind::Name, 4..7),
        (TokenKind::Lpar, 7..8),
        (TokenKind::Rpar, 8..9),
        (TokenKind::Colon, 9..10),
        (TokenKind::Newline, 10..11),
        // Gap               ||..||
        (TokenKind::Comment, 15..24),
        (TokenKind::NonLogicalNewline, 24..25),
        (TokenKind::Indent, 25..29),
        (TokenKind::Pass, 29..33),
        // No newline at the end to keep the token set full of unique tokens
    ];

    /// Helper function to create [`Tokens`] from an iterator of (kind, range).
    fn new_tokens(tokens: impl Iterator<Item = (TokenKind, Range<u32>)>) -> Tokens {
        Tokens::new(
            tokens
                .map(|(kind, range)| {
                    Token::new(
                        kind,
                        TextRange::new(TextSize::new(range.start), TextSize::new(range.end)),
                        TokenFlags::empty(),
                    )
                })
                .collect(),
        )
    }

    #[test]
    fn tokens_after_offset_at_token_start() {
        let tokens = new_tokens(TEST_CASE_WITH_GAP.into_iter());
        let after = tokens.after(TextSize::new(8));
        assert_eq!(after.len(), 7);
        assert_eq!(after.first().unwrap().kind(), TokenKind::Rpar);
    }

    #[test]
    fn tokens_after_offset_at_token_end() {
        let tokens = new_tokens(TEST_CASE_WITH_GAP.into_iter());
        let after = tokens.after(TextSize::new(11));
        assert_eq!(after.len(), 4);
        assert_eq!(after.first().unwrap().kind(), TokenKind::Comment);
    }

    #[test]
    fn tokens_after_offset_between_tokens() {
        let tokens = new_tokens(TEST_CASE_WITH_GAP.into_iter());
        let after = tokens.after(TextSize::new(13));
        assert_eq!(after.len(), 4);
        assert_eq!(after.first().unwrap().kind(), TokenKind::Comment);
    }

    #[test]
    fn tokens_after_offset_at_last_token_end() {
        let tokens = new_tokens(TEST_CASE_WITH_GAP.into_iter());
        let after = tokens.after(TextSize::new(33));
        assert_eq!(after.len(), 0);
    }

    #[test]
    #[should_panic(expected = "Offset 5 is inside a token range 4..7")]
    fn tokens_after_offset_inside_token() {
        let tokens = new_tokens(TEST_CASE_WITH_GAP.into_iter());
        tokens.after(TextSize::new(5));
    }

    #[test]
    fn tokens_in_range_at_token_offset() {
        let tokens = new_tokens(TEST_CASE_WITH_GAP.into_iter());
        let in_range = tokens.in_range(TextRange::new(4.into(), 10.into()));
        assert_eq!(in_range.len(), 4);
        assert_eq!(in_range.first().unwrap().kind(), TokenKind::Name);
        assert_eq!(in_range.last().unwrap().kind(), TokenKind::Colon);
    }

    #[test]
    fn tokens_in_range_start_offset_at_token_end() {
        let tokens = new_tokens(TEST_CASE_WITH_GAP.into_iter());
        let in_range = tokens.in_range(TextRange::new(11.into(), 29.into()));
        assert_eq!(in_range.len(), 3);
        assert_eq!(in_range.first().unwrap().kind(), TokenKind::Comment);
        assert_eq!(in_range.last().unwrap().kind(), TokenKind::Indent);
    }

    #[test]
    fn tokens_in_range_end_offset_at_token_start() {
        let tokens = new_tokens(TEST_CASE_WITH_GAP.into_iter());
        let in_range = tokens.in_range(TextRange::new(8.into(), 15.into()));
        assert_eq!(in_range.len(), 3);
        assert_eq!(in_range.first().unwrap().kind(), TokenKind::Rpar);
        assert_eq!(in_range.last().unwrap().kind(), TokenKind::Newline);
    }

    #[test]
    fn tokens_in_range_start_offset_between_tokens() {
        let tokens = new_tokens(TEST_CASE_WITH_GAP.into_iter());
        let in_range = tokens.in_range(TextRange::new(13.into(), 29.into()));
        assert_eq!(in_range.len(), 3);
        assert_eq!(in_range.first().unwrap().kind(), TokenKind::Comment);
        assert_eq!(in_range.last().unwrap().kind(), TokenKind::Indent);
    }

    #[test]
    fn tokens_in_range_end_offset_between_tokens() {
        let tokens = new_tokens(TEST_CASE_WITH_GAP.into_iter());
        let in_range = tokens.in_range(TextRange::new(9.into(), 13.into()));
        assert_eq!(in_range.len(), 2);
        assert_eq!(in_range.first().unwrap().kind(), TokenKind::Colon);
        assert_eq!(in_range.last().unwrap().kind(), TokenKind::Newline);
    }

    #[test]
    #[should_panic(expected = "Offset 5 is inside a token range 4..7")]
    fn tokens_in_range_start_offset_inside_token() {
        let tokens = new_tokens(TEST_CASE_WITH_GAP.into_iter());
        tokens.in_range(TextRange::new(5.into(), 10.into()));
    }

    #[test]
    #[should_panic(expected = "End offset 6 is inside a token range 4..7")]
    fn tokens_in_range_end_offset_inside_token() {
        let tokens = new_tokens(TEST_CASE_WITH_GAP.into_iter());
        tokens.in_range(TextRange::new(0.into(), 6.into()));
    }
}
