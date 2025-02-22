use anyhow::Ok;
use lsp_types::TextEdit;
use ruff_source_file::LineIndex;
use rustc_hash::FxHashMap;

use crate::{session::DocumentSnapshot, util::run_ruff_fix};

use super::{Replacement, ToRangeExt};

/// A simultaneous fix made across a single text document.
pub(crate) type Fixes = FxHashMap<lsp_types::Url, Vec<lsp_types::TextEdit>>;

pub(crate) fn fix_diagnostics<'a, I>(
    snapshot: &'a DocumentSnapshot,
    extra_args: I,
) -> crate::Result<Fixes>
where
    I: IntoIterator<Item = &'a str>,
{
    let Some(ruff_path) = snapshot.client_settings().ruff_path() else {
        tracing::warn!("Ruff path was not set in settings!");
        show_warn_msg!("Ruff path was not set in settings!");
        return Ok(Fixes::default());
    };

    let original_content = snapshot.document().contents();
    let (modified_content, output) = run_ruff_fix(
        ruff_path,
        snapshot.url().as_str(),
        original_content,
        extra_args,
    )?;

    if output.status.code().is_some_and(|code| code == 2) {
        let msg = String::from_utf8_lossy(&output.stderr);
        show_err_msg!("Ruff: Fix failed ({})", msg);
        return Ok(Fixes::default());
    }

    // If the content wasn't modified no fix were applied
    if original_content == modified_content {
        return Ok(Fixes::default());
    }

    let original_index = LineIndex::from_source_text(original_content);
    let modified_index = LineIndex::from_source_text(&modified_content);

    let Replacement {
        source_range,
        modified_range,
    } = Replacement::between(
        original_content,
        original_index.line_starts(),
        &modified_content,
        modified_index.line_starts(),
    );

    Ok([(
        snapshot.url().clone(),
        vec![TextEdit {
            range: source_range.to_range(original_content, &original_index, snapshot.encoding()),
            new_text: modified_content[modified_range].to_owned(),
        }],
    )]
    .into_iter()
    .collect())
}
