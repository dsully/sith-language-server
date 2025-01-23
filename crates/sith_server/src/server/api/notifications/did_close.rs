use crate::server::api::diagnostics::clear_diagnostics_for_document;
use crate::server::api::LSPResult;
use crate::server::client::Notifier;
use crate::server::Result;
use crate::session::Session;
use lsp_types as types;
use lsp_types::notification as notif;

pub(crate) struct DidClose;

impl super::NotificationHandler for DidClose {
    type NotificationType = notif::DidCloseTextDocument;
}

impl super::SyncNotificationHandler for DidClose {
    #[tracing::instrument(skip_all, fields(file=%uri))]
    fn run(
        session: &mut Session,
        notifier: Notifier,
        types::DidCloseTextDocumentParams {
            text_document: types::TextDocumentIdentifier { uri },
        }: types::DidCloseTextDocumentParams,
    ) -> Result<()> {
        let Some(snapshot) = session.take_snapshot(&uri) else {
            tracing::debug!("Unable to take snapshot for document with URL {uri}");
            return Ok(());
        };

        clear_diagnostics_for_document(&snapshot, &notifier)?;

        session
            .close_document(&uri)
            .with_failure_code(lsp_server::ErrorCode::InternalError)
    }
}
