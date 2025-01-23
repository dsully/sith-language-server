pub use edit::{Document, PositionEncoding};
pub use server::Server;

mod edit;
mod server;
mod session;
mod trace;

pub(crate) const SERVER_NAME: &str = "SithLSP";
pub(crate) const DIAGNOSTIC_NAME: &str = "Sith";

/// A common result type used in most cases where a
/// result type is needed.
pub(crate) type Result<T> = anyhow::Result<T>;
