use std::str::FromStr;

use lsp_server::ErrorCode;
use lsp_types::{self as types, request as req};
use serde::Deserialize;

use crate::edit::{DocumentVersion, WorkspaceEditTracker};
use crate::server::api::LSPResult;
use crate::server::client::Requester;
use crate::server::{Result, SupportedCommand, Task};
use crate::SERVER_NAME;
use crate::{server::client::Notifier, session::Session};

#[derive(Deserialize)]
struct Argument {
    uri: types::Url,
    version: DocumentVersion,
}

pub(crate) struct ExecuteCommand;

impl super::RequestHandler for ExecuteCommand {
    type RequestType = req::ExecuteCommand;
}

impl super::SyncRequestHandler for ExecuteCommand {
    fn run(
        session: &mut Session,
        _notifier: Notifier,
        requester: &mut Requester,
        params: types::ExecuteCommandParams,
    ) -> Result<Option<serde_json::Value>> {
        let command = SupportedCommand::from_str(&params.command)
            .with_failure_code(ErrorCode::InvalidParams)?;

        // check if we can apply a workspace edit
        if !session.resolved_client_capabilities().apply_edit {
            return Err(anyhow::anyhow!("Cannot execute the '{}' command: the client does not support `workspace/applyEdit`", command.label())).with_failure_code(ErrorCode::InternalError);
        }

        let mut arguments: Vec<Argument> = params
            .arguments
            .into_iter()
            .map(|value| serde_json::from_value(value).with_failure_code(ErrorCode::InvalidParams))
            .collect::<Result<_>>()?;

        arguments.sort_by(|a, b| a.uri.cmp(&b.uri));
        arguments.dedup_by(|a, b| a.uri == b.uri);

        let mut edit_tracker = WorkspaceEditTracker::new(session.resolved_client_capabilities());
        for Argument { uri, version } in arguments {
            let Some(snapshot) = session.take_snapshot(&uri) else {
                tracing::error!("Document at {uri} could not be opened");
                show_err_msg!("Ruff does not recognize this file");
                return Ok(None);
            };
            match command {
                SupportedCommand::FixAll => {
                    let fixes = super::code_action_resolve::fix_all_edit(&snapshot)
                        .with_failure_code(ErrorCode::InternalError)?;
                    edit_tracker
                        .set_fixes_for_document(fixes, snapshot.document_version())
                        .with_failure_code(ErrorCode::InternalError)?;
                }
                SupportedCommand::Format => {
                    if let Some(fixes) = super::format::format_full_document(&snapshot)? {
                        edit_tracker
                            .set_fixes_for_document(fixes, version)
                            .with_failure_code(ErrorCode::InternalError)?;
                    }
                }
                SupportedCommand::OrganizeImports => {
                    let fixes = super::code_action_resolve::organize_imports_edit(&snapshot)
                        .with_failure_code(ErrorCode::InternalError)?;
                    edit_tracker
                        .set_fixes_for_document(fixes, snapshot.document_version())
                        .with_failure_code(ErrorCode::InternalError)?;
                }
            }
        }

        if !edit_tracker.is_empty() {
            apply_edit(
                requester,
                command.label(),
                edit_tracker.into_workspace_edit(),
            )
            .with_failure_code(ErrorCode::InternalError)?;
        }

        Ok(None)
    }
}

fn apply_edit(
    requester: &mut Requester,
    label: &str,
    edit: types::WorkspaceEdit,
) -> crate::Result<()> {
    requester.request::<req::ApplyWorkspaceEdit>(
        types::ApplyWorkspaceEditParams {
            label: Some(format!("{SERVER_NAME}: {label}")),
            edit,
        },
        |response| {
            if !response.applied {
                let reason = response
                    .failure_reason
                    .unwrap_or_else(|| String::from("unspecified reason"));
                tracing::error!("Failed to apply workspace edit: {reason}");
                show_err_msg!("Ruff was unable to apply edits: {reason}");
            }
            Task::nothing()
        },
    )
}
