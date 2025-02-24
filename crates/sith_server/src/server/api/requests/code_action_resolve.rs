use std::borrow::Cow;

use crate::{
    edit::{
        fix::{fix_diagnostics, Fixes},
        WorkspaceEditTracker,
    },
    server::{api::LSPResult, client::Notifier, Result, SupportedCodeAction},
    session::DocumentSnapshot,
};
use lsp_server::ErrorCode;
use lsp_types::{self as types, request as req};

pub(crate) struct CodeActionResolve;

impl super::RequestHandler for CodeActionResolve {
    type RequestType = req::CodeActionResolveRequest;
}

impl super::BackgroundDocumentRequestHandler for CodeActionResolve {
    fn document_url(params: &types::CodeAction) -> Cow<types::Url> {
        let uri: lsp_types::Url = serde_json::from_value(params.data.clone().unwrap_or_default())
            .expect("code actions should have a URI in their data fields");
        Cow::Owned(uri)
    }

    fn run_with_snapshot(
        snapshot: DocumentSnapshot,
        _notifier: Notifier,
        mut action: types::CodeAction,
    ) -> Result<types::CodeAction> {
        let code_actions = SupportedCodeAction::from_kind(
            action
                .kind
                .clone()
                .ok_or(anyhow::anyhow!("No kind was given for code action"))
                .with_failure_code(ErrorCode::InvalidParams)?,
        )
        .collect::<Vec<_>>();

        // Ensure that the code action maps to _exactly one_ supported code action
        let [action_kind] = code_actions.as_slice() else {
            return Err(anyhow::anyhow!(
                "Code action resolver did not expect code action kind {:?}",
                action.kind.as_ref().unwrap()
            ))
            .with_failure_code(ErrorCode::InvalidParams);
        };

        action.edit = match action_kind {
            SupportedCodeAction::SourceOrganizeImports => Some(
                resolve_edit_for_organize_imports(&snapshot)
                    .with_failure_code(ErrorCode::InternalError)?,
            ),
            SupportedCodeAction::SourceFixAll => Some(
                resolve_edit_for_fix_all(&snapshot).with_failure_code(ErrorCode::InternalError)?,
            ),
            SupportedCodeAction::QuickFix => {
                // The client may ask us to resolve a code action, as it has no way of knowing
                // whether e.g. `command` field will be filled out by the resolution callback.
                return Ok(action);
            }
        };

        Ok(action)
    }
}

pub(super) fn resolve_edit_for_fix_all(
    snapshot: &DocumentSnapshot,
) -> crate::Result<types::WorkspaceEdit> {
    let mut tracker = WorkspaceEditTracker::new(snapshot.resolved_client_capabilities());
    tracker.set_fixes_for_document(fix_all_edit(snapshot)?, snapshot.document_version())?;
    Ok(tracker.into_workspace_edit())
}

pub(super) fn fix_all_edit(snapshot: &DocumentSnapshot) -> crate::Result<Fixes> {
    fix_diagnostics(snapshot, [])
}

pub(super) fn resolve_edit_for_organize_imports(
    snapshot: &DocumentSnapshot,
) -> crate::Result<types::WorkspaceEdit> {
    let mut tracker = WorkspaceEditTracker::new(snapshot.resolved_client_capabilities());
    tracker.set_fixes_for_document(
        organize_imports_edit(snapshot)?,
        snapshot.document_version(),
    )?;
    Ok(tracker.into_workspace_edit())
}

pub(super) fn organize_imports_edit(snapshot: &DocumentSnapshot) -> crate::Result<Fixes> {
    #[rustfmt::skip]
    let args = [
            "--select",
            "I001", // unsorted imports
            "I002", // missing required imports

    ];
    fix_diagnostics(snapshot, args)
}
