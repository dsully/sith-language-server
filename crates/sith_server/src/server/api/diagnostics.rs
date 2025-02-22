use std::path::PathBuf;

use anyhow::Context;
use lsp_server::ErrorCode;
use lsp_types::{
    CodeDescription, Diagnostic, DiagnosticSeverity, NumberOrString, Position, Range, Url,
};
use semantic_model::declaration::{DeclStmt, DeclarationKind};
use serde::{Deserialize, Serialize};

use crate::{
    edit::ToRangeExt,
    server::{api::LSPResult, Result},
    util::{run_ruff_check, UNSUPPORTED_CHECK_ARGS},
};
use crate::{server::client::Notifier, session::DocumentSnapshot};

#[derive(Deserialize, Serialize, Debug)]
pub(crate) struct RuffLintLocation {
    pub(crate) column: u32,
    pub(crate) row: u32,
}

impl RuffLintLocation {
    fn lsp_position(&self) -> lsp_types::Position {
        Position {
            // The lint location is one-based but the lsp_types::Position is zero-based
            line: self.row - 1,
            character: self.column - 1,
        }
    }
}

#[derive(Deserialize, Serialize, Debug)]
pub(crate) struct FixEdit {
    pub(crate) content: String,
    pub(crate) location: RuffLintLocation,
    pub(crate) end_location: RuffLintLocation,
}

impl FixEdit {
    pub(crate) fn lsp_range(&self) -> lsp_types::Range {
        Range {
            start: self.location.lsp_position(),
            end: self.end_location.lsp_position(),
        }
    }
}

#[derive(Deserialize, Serialize, Debug)]
pub(crate) struct LintFix {
    pub(crate) applicability: String,
    pub(crate) edits: Vec<FixEdit>,
}

#[derive(Deserialize, Serialize, Debug)]
pub(crate) struct RuffLintDiagnostic {
    pub(crate) filename: PathBuf,
    pub(crate) message: String,
    pub(crate) code: Option<String>,
    pub(crate) location: RuffLintLocation,
    pub(crate) end_location: RuffLintLocation,
    pub(crate) url: Option<Url>,
    pub(crate) fix: Option<LintFix>,
}

impl RuffLintDiagnostic {
    pub(super) fn lsp_range(&self) -> lsp_types::Range {
        Range {
            start: self.location.lsp_position(),
            end: self.end_location.lsp_position(),
        }
    }
}

pub(super) fn generate_sith_diagnostics(snapshot: &DocumentSnapshot) -> Vec<Diagnostic> {
    let document_path = snapshot.url().to_file_path().expect("to be a filepath");
    let db = snapshot.db();
    let document = snapshot.document();
    let ast = db.indexer().ast(&document_path);

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
    let table = db.table(&document_path);
    diagnostics.extend(
        table
            .declarations()
            .filter(|declaration| match &declaration.kind {
                DeclarationKind::Stmt(
                    DeclStmt::Import {
                        stub_source: None,
                        non_stub_source: None,
                    }
                    | DeclStmt::ImportSegment {
                        stub_source: None,
                        non_stub_source: None,
                    },
                ) => true,
                DeclarationKind::Stmt(DeclStmt::SameImport(decl_id)) => db
                    .declaration(&document_path, *decl_id)
                    .import_source()
                    .is_none(),
                _ => false,
            })
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

pub(super) fn generate_ruff_lint_diagnostics(
    snapshot: &DocumentSnapshot,
) -> Result<Vec<Diagnostic>> {
    let settings = snapshot.client_settings();

    let Some(ruff_path) = settings.ruff_path() else {
        tracing::warn!("Ruff path was not set in settings!");
        show_warn_msg!("Ruff path was not set in settings!");
        return Ok(vec![]);
    };

    let mut args = settings
        .lint_args()
        .iter()
        .filter(|arg| UNSUPPORTED_CHECK_ARGS.contains(&arg.as_str()))
        .cloned()
        .collect::<Vec<_>>();
    if let Some(select) = settings.lint_select() {
        args.extend(["--select".into(), select.join(",")]);
    }
    if let Some(extend_select) = settings.lint_extend_select() {
        args.extend(["--extend-select".into(), extend_select.join(",")]);
    }
    if let Some(ignore) = settings.lint_ignore() {
        if !ignore.is_empty() {
            args.extend(["--ignore".into(), ignore.join(",")]);
        }
    }

    let output = run_ruff_check(
        ruff_path,
        snapshot.url().as_str(),
        snapshot.document().contents(),
        args.iter().map(String::as_str),
    )
    .with_failure_code(ErrorCode::InternalError)?;

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
            range: lint_diagnostic.lsp_range(),
            message: lint_diagnostic.message,
            code: Some(NumberOrString::String(lint_diagnostic.code.unwrap())),
            severity: Some(DiagnosticSeverity::WARNING),
            source: Some("ruff".into()),
            code_description: Some(CodeDescription {
                href: lint_diagnostic.url.unwrap(),
            }),
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
