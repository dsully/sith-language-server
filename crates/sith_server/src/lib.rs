pub use edit::{Document, PositionEncoding};
use lsp_types::CodeActionKind;
pub use server::Server;

#[macro_use]
mod message;

mod edit;
mod server;
mod session;
mod trace;
mod util;

pub(crate) const VERSION: &str = env!("CARGO_PKG_VERSION");
pub(crate) const SERVER_NAME: &str = "SithLSP";

pub(crate) const SOURCE_ORGANIZE_IMPORTS_SITH: CodeActionKind =
    CodeActionKind::new("source.organizeImports.sith");
pub(crate) const SOURCE_FIX_ALL_SITH: CodeActionKind = CodeActionKind::new("source.fixAll.sith");

/// A common result type used in most cases where a
/// result type is needed.
pub(crate) type Result<T> = anyhow::Result<T>;
