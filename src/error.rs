use crate::lexer::span::Span;

pub type PythonErrors = Option<Vec<PythonError>>;

#[derive(Debug, PartialEq)]
pub struct PythonError {
    pub error: PythonErrorType,
    pub msg: String,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub enum PythonErrorType {
    Syntax,
    Indentation,
}
