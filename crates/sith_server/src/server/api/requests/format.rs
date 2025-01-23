use std::{
    io::Write,
    process::{Command, Stdio},
};

use crate::{
    edit::{Replacement, ToRangeExt},
    server::{api::LSPResult, client::Notifier, Result},
    session::DocumentSnapshot,
};
use anyhow::Context;
use lsp_types::{self as types, request as req, TextEdit};
use ruff_source_file::LineIndex;

// Arguments that are not allowed to be passed to `ruff format`.
const UNSUPPORTED_FORMAT_ARGS: &[&str] = &[
    "--force-exclude",
    "--quiet",
    "-h",
    "--help",
    "--no-force-exclude",
    "--silent",
    "--verbose",
    "--stdin-filename",
];

pub(crate) struct Format;

impl super::RequestHandler for Format {
    type RequestType = req::Formatting;
}

impl super::BackgroundDocumentRequestHandler for Format {
    super::define_document_url!(params: &types::DocumentFormattingParams);
    fn run_with_snapshot(
        snapshot: DocumentSnapshot,
        _notifier: Notifier,
        params: types::DocumentFormattingParams,
    ) -> Result<super::FormatResponse> {
        let settings = snapshot.client_settings();
        if !settings.is_format_enabled() {
            return Ok(None);
        }

        let document = snapshot.document();
        let Some(ruff_path) = settings.ruff_path() else {
            tracing::error!("Ruff path was not set!");
            return Ok(None);
        };
        let mut child = Command::new(ruff_path)
            .arg("format")
            .args(
                [
                    "--force-exclude",
                    "--quiet",
                    "--stdin-filename",
                    params.text_document.uri.as_ref(),
                ]
                .into_iter()
                .chain(remove_unsupported_format_args(settings.format_args())),
            )
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .context("Failed to execute Ruff")
            .with_failure_code(lsp_server::ErrorCode::InternalError)?;

        let source = document.contents().to_string();
        let mut stdin = child
            .stdin
            .take()
            .context("Failed to open stdin")
            .with_failure_code(lsp_server::ErrorCode::InternalError)?;
        std::thread::spawn(move || {
            stdin
                .write_all(source.as_bytes())
                .context("Failed to write to stdin")
                .unwrap()
        });

        let output = child
            .wait_with_output()
            .context("Failed to wait on child")
            .with_failure_code(lsp_server::ErrorCode::InternalError)?;

        if output.status.code().is_some_and(|code| code != 0) {
            return Err(anyhow::anyhow!(
                String::from_utf8_lossy(&output.stderr).to_string()
            ))
            .context("Ruff formatting failed")
            .with_failure_code(lsp_server::ErrorCode::InternalError);
        }
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

        Ok(Some(vec![TextEdit {
            range: source_range.to_range(source, unformatted_index, snapshot.encoding()),
            new_text: formatted[formatted_range].to_owned(),
        }]))
    }
}

fn remove_unsupported_format_args(args: &[String]) -> impl Iterator<Item = &str> {
    args.iter().filter_map(|arg| {
        if UNSUPPORTED_FORMAT_ARGS.contains(&arg.as_str()) {
            tracing::info!("Ignoring unsupported argument: {arg}");
            None
        } else {
            Some(arg.as_str())
        }
    })
}
