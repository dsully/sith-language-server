mod completion;
mod diagnostic;
mod document_highlight;
mod format;
mod goto_definition;
mod hover;
mod prepare_rename;
mod references;

use super::{
    define_document_url,
    traits::{BackgroundDocumentRequestHandler, RequestHandler},
};
pub(super) use completion::resolve::ResolveCompletionItem;
pub(super) use completion::Completion;
pub(super) use diagnostic::DocumentDiagnostic;
pub(super) use document_highlight::DocumentHighlight;
pub(super) use format::Format;
pub(super) use goto_definition::GotoDefinition;
pub(super) use hover::Hover;
pub(super) use prepare_rename::PrepareRename;
pub(super) use references::References;

type FormatResponse = Option<Vec<lsp_types::TextEdit>>;
