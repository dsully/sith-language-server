pub(super) mod code_action;
pub(super) mod code_action_resolve;
pub(super) mod completion;
pub(super) mod diagnostic;
pub(super) mod document_highlight;
pub(super) mod document_symbol;
pub(super) mod execute_command;
pub(super) mod format;
pub(super) mod goto_definition;
pub(super) mod hover;
pub(super) mod prepare_rename;
pub(super) mod references;
pub(super) mod rename;
pub(super) mod signature_help;

use super::{
    define_document_url,
    traits::{BackgroundDocumentRequestHandler, RequestHandler, SyncRequestHandler},
};
pub(super) use code_action::CodeActions;
pub(super) use code_action_resolve::CodeActionResolve;
pub(super) use completion::resolve::ResolveCompletionItem;
pub(super) use completion::Completion;
pub(super) use diagnostic::DocumentDiagnostic;
pub(super) use document_highlight::DocumentHighlight;
pub(super) use document_symbol::DocumentSymbol;
pub(super) use execute_command::ExecuteCommand;
pub(super) use format::Format;
pub(super) use goto_definition::GotoDefinition;
pub(super) use hover::Hover;
pub(super) use prepare_rename::PrepareRename;
pub(super) use references::References;
pub(super) use rename::Rename;
pub(super) use signature_help::SignatureHelp;

type FormatResponse = Option<Vec<lsp_types::TextEdit>>;
