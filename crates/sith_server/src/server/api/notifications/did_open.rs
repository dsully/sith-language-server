use crate::server::api::diagnostics::publish_diagnostics_for_document;
use crate::server::api::LSPResult;
use crate::server::client::Notifier;
use crate::server::Result;
use crate::session::Session;
use lsp_types as types;
use lsp_types::notification as notif;

pub(crate) struct DidOpen;

impl super::NotificationHandler for DidOpen {
    type NotificationType = notif::DidOpenTextDocument;
}

impl super::SyncNotificationHandler for DidOpen {
    #[tracing::instrument(skip_all, fields(file=%uri))]
    fn run(
        session: &mut Session,
        notifier: Notifier,
        types::DidOpenTextDocumentParams {
            text_document:
                types::TextDocumentItem {
                    uri, text, version, ..
                },
        }: types::DidOpenTextDocumentParams,
    ) -> Result<()> {
        session.open_document(&uri, text, version);

        // Publish diagnostics if the client doesnt support pull diagnostics
        if !session.resolved_client_capabilities().pull_diagnostics {
            let snapshot = session
                .take_snapshot(&uri)
                .ok_or_else(|| {
                    anyhow::anyhow!("Unable to take snapshot for document with URL {uri}")
                })
                .with_failure_code(lsp_server::ErrorCode::InternalError)?;
            publish_diagnostics_for_document(&snapshot, &notifier)?;
        }

        Ok(())
    }
}
