use crate::{
    edit::{fix::Fixes, Replacement, ToRangeExt},
    server::{api::LSPResult, client::Notifier, Result},
    session::DocumentSnapshot,
    util::{run_ruff_format, UNSUPPORTED_FORMAT_ARGS},
};
use anyhow::Context;
use lsp_types::{self as types, request as req, TextEdit};
use ruff_source_file::LineIndex;

pub(crate) struct Format;

impl super::RequestHandler for Format {
    type RequestType = req::Formatting;
}

impl super::BackgroundDocumentRequestHandler for Format {
    super::define_document_url!(params: &types::DocumentFormattingParams);
    fn run_with_snapshot(
        snapshot: DocumentSnapshot,
        _notifier: Notifier,
        _params: types::DocumentFormattingParams,
    ) -> Result<super::FormatResponse> {
        Ok(format_full_document(&snapshot)?.and_then(|fixes| fixes.into_values().next()))
    }
}

pub(super) fn format_full_document(snapshot: &DocumentSnapshot) -> Result<Option<Fixes>> {
    let mut fixes = Fixes::default();

    let settings = snapshot.client_settings();
    if !settings.is_format_enabled() {
        return Ok(None);
    }

    let Some(ruff_path) = settings.ruff_path() else {
        tracing::warn!("Ruff path was not set in settings!");
        show_warn_msg!("Ruff path was not set in settings!");
        return Ok(None);
    };

    let args = settings
        .format_args()
        .iter()
        .filter(|&arg| UNSUPPORTED_FORMAT_ARGS.contains(&arg.as_str()))
        .cloned()
        .collect::<Vec<_>>();
    let output = run_ruff_format(
        ruff_path,
        snapshot.url().as_str(),
        snapshot.document().contents(),
        args.iter().map(String::as_str),
    )
    .with_failure_code(lsp_server::ErrorCode::InternalError)?;

    if output.status.code().is_some_and(|code| code != 0) {
        return Err(anyhow::anyhow!(
            String::from_utf8_lossy(&output.stderr).to_string()
        ))
        .context("Ruff formatting failed")
        .with_failure_code(lsp_server::ErrorCode::InternalError);
    }

    let document = snapshot.document();
    let source = document.contents();
    let formatted = String::from_utf8_lossy(&output.stdout);

    let formatted_index = LineIndex::from_source_text(&formatted);
    let unformatted_index = document.index();

    let Replacement {
        source_range,
        modified_range: formatted_range,
    } = Replacement::between(
        source,
        unformatted_index.line_starts(),
        &formatted,
        formatted_index.line_starts(),
    );

    fixes.insert(
        snapshot.url().clone(),
        vec![TextEdit {
            range: source_range.to_range(source, unformatted_index, snapshot.encoding()),
            new_text: formatted[formatted_range].to_owned(),
        }],
    );

    Ok(Some(fixes))
}
