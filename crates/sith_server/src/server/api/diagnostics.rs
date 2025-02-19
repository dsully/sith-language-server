use std::{
    io::Write,
    path::PathBuf,
    process::{Command, Stdio},
};

use anyhow::Context;
use lsp_types::{
    CodeDescription, Diagnostic, DiagnosticSeverity, NumberOrString, Position, Range, Url,
};
use serde::{Deserialize, Serialize};

use crate::{
    edit::ToRangeExt,
    server::{api::LSPResult, Result},
};
use crate::{server::client::Notifier, session::DocumentSnapshot};

#[derive(Deserialize, Serialize, Debug)]
struct RuffLintLocation {
    column: u32,
    row: u32,
}

#[derive(Deserialize, Serialize, Debug)]
struct FixEdit {
    content: String,
    location: RuffLintLocation,
    end_location: RuffLintLocation,
}

#[derive(Deserialize, Serialize, Debug)]
struct LintFix {
    applicability: String,
    edits: Vec<FixEdit>,
}

// TODO: add support for fixing lints
#[derive(Deserialize, Serialize, Debug)]
struct RuffLintDiagnostic {
    filename: PathBuf,
    message: String,
    code: Option<String>,
    location: RuffLintLocation,
    end_location: RuffLintLocation,
    url: Option<Url>,
    fix: Option<LintFix>,
}

pub(super) fn generate_sith_diagnostics(snapshot: &DocumentSnapshot) -> Vec<Diagnostic> {
    let db = snapshot.db();
    let document = snapshot.document();
    let ast = db
        .indexer()
        .ast(&snapshot.url().to_file_path().expect("to be a filepath"));

    let mut diagnostics = Vec::new();
    let create_diagnostic = |message, range, severity, source| Diagnostic {
        message,
        range,
        severity: Some(severity),
        source: Some(source),
        ..Default::default()
    };

    // Syntax errors diagnostic
    diagnostics.extend(ast.errors().iter().map(|syntax_error| {
        create_diagnostic(
            syntax_error.to_string(),
            syntax_error.location.to_range(
                document.contents(),
                document.index(),
                snapshot.encoding(),
            ),
            DiagnosticSeverity::ERROR,
            crate::SERVER_NAME.into(),
        )
    }));

    // Unresolved imports diagnostic
    let table = db.table(&snapshot.url().to_file_path().expect("to be a filepath"));
    diagnostics.extend(
        table
            .declarations()
            .filter(|declaration| declaration.is_import() && declaration.import_source().is_none())
            .map(|declaration| {
                create_diagnostic(
                    "Failed to resolve import".into(),
                    declaration.range.to_range(
                        document.contents(),
                        document.index(),
                        snapshot.encoding(),
                    ),
                    DiagnosticSeverity::ERROR,
                    crate::SERVER_NAME.into(),
                )
            }),
    );

    diagnostics
}

const UNSUPPORTED_CHECK_ARGS: &[&str] = &[
    "--force-exclude",
    "--no-cache",
    "--no-fix",
    "--quiet",
    "--diff",
    "--exit-non-zero-on-fix",
    "-e",
    "--exit-zero",
    "--fix",
    "--fix-only",
    "-h",
    "--help",
    "--no-force-exclude",
    "--show-files",
    "--show-fixes",
    "--show-settings",
    "--show-source",
    "--silent",
    "--statistics",
    "--verbose",
    "-w",
    "--watch",
    "--stdin-filename",
    "--output-format",
];

pub(super) fn generate_ruff_lint_diagnostics(
    snapshot: &DocumentSnapshot,
) -> Result<Vec<Diagnostic>> {
    let settings = snapshot.client_settings();

    let Some(ruff_path) = settings.ruff_path() else {
        tracing::warn!("Ruff path was not set in settings!");
        show_warn_msg!("Ruff path was not set in settings!");
        return Ok(vec![]);
    };

    let mut cmd = Command::new(ruff_path);
    cmd.arg("check").args(
        [
            "--no-fix",
            "--force-exclude",
            "--quiet",
            "--output-format",
            "json",
            "--stdin-filename",
            snapshot.url().as_str(),
        ]
        .into_iter()
        .chain(remove_unsupported_check_args(settings.lint_args())),
    );

    if let Some(select) = settings.lint_select() {
        cmd.arg("--select").arg(select.join(","));
    }

    if let Some(extend_select) = settings.lint_extend_select() {
        cmd.arg("--extend-select").arg(extend_select.join(","));
    }

    if let Some(ignore) = settings.lint_ignore() {
        if !ignore.is_empty() {
            cmd.args(["--ignore", &ignore.join(",")]);
        }
    }

    let mut child = cmd
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .context("Failed to execute Ruff")
        .with_failure_code(lsp_server::ErrorCode::InternalError)?;

    let contents = snapshot.document().contents().to_string();
    let mut stdin = child
        .stdin
        .take()
        .context("Failed to open stdin")
        .with_failure_code(lsp_server::ErrorCode::InternalError)?;

    std::thread::spawn(move || {
        stdin
            .write_all(contents.as_bytes())
            .context("Failed to write to stdin")
            .unwrap()
    });
    let output = child
        .wait_with_output()
        .context("Failed to wait on child")
        .with_failure_code(lsp_server::ErrorCode::InternalError)?;

    if output.status.code().is_some_and(|code| code == 2) {
        return Err(anyhow::anyhow!(
            String::from_utf8_lossy(&output.stderr).to_string()
        ))
        .context("Ruff: Lint failed")
        .with_failure_code(lsp_server::ErrorCode::InternalError);
    }

    let json_output = String::from_utf8_lossy(&output.stdout);
    let result: Vec<RuffLintDiagnostic> =
        serde_json::from_str(&json_output).expect("failed to deserialize JSON");

    // ignore ruff diagnostics for syntax errors
    if result
        .iter()
        .any(|diagnostic| diagnostic.message.starts_with("SyntaxError:"))
    {
        return Ok(vec![]);
    }

    Ok(result
        .into_iter()
        .map(|lint_diagnostic| Diagnostic {
            message: lint_diagnostic.message,
            code: Some(NumberOrString::String(lint_diagnostic.code.unwrap())),
            severity: Some(DiagnosticSeverity::WARNING),
            source: Some("ruff".into()),
            code_description: Some(CodeDescription {
                href: lint_diagnostic.url.unwrap(),
            }),
            range: Range {
                start: Position {
                    line: lint_diagnostic.location.row - 1,
                    character: lint_diagnostic.location.column - 1,
                },
                end: Position {
                    line: lint_diagnostic.end_location.row - 1,
                    character: lint_diagnostic.end_location.column - 1,
                },
            },
            data: lint_diagnostic
                .fix
                .map(|fix| serde_json::to_value(fix).unwrap()),
            ..Default::default()
        })
        .collect())
}

pub(super) fn publish_diagnostics_for_document(
    snapshot: &DocumentSnapshot,
    notifier: &Notifier,
) -> Result<()> {
    let sith_diagnostics = generate_sith_diagnostics(snapshot);
    let ruff_lint_diagnostics = generate_ruff_lint_diagnostics(snapshot)?;
    for diagnostics in [sith_diagnostics, ruff_lint_diagnostics] {
        notifier
            .notify::<lsp_types::notification::PublishDiagnostics>(
                lsp_types::PublishDiagnosticsParams {
                    uri: snapshot.url().clone(),
                    diagnostics,
                    version: Some(snapshot.document_version()),
                },
            )
            .with_failure_code(lsp_server::ErrorCode::InternalError)?;
    }

    Ok(())
}

pub(super) fn clear_diagnostics_for_document(
    snapshot: &DocumentSnapshot,
    notifier: &Notifier,
) -> crate::server::Result<()> {
    notifier
        .notify::<lsp_types::notification::PublishDiagnostics>(
            lsp_types::PublishDiagnosticsParams {
                uri: snapshot.url().clone(),
                diagnostics: vec![],
                version: Some(snapshot.document_version()),
            },
        )
        .with_failure_code(lsp_server::ErrorCode::InternalError)?;

    Ok(())
}

fn remove_unsupported_check_args(args: &[String]) -> impl Iterator<Item = &str> {
    args.iter().filter_map(|arg| {
        if UNSUPPORTED_CHECK_ARGS.contains(&arg.as_str()) {
            tracing::info!("Ignoring unsupported argument: {arg}");
            None
        } else {
            Some(arg.as_str())
        }
    })
}
