use lsp_types::{self as types, request as req};
use python_ast::AnyNodeRef;
use python_ast_utils::node_at_offset;
use ruff_text_size::{Ranged, TextRange, TextSize};

use crate::{
    edit::{position_to_offset, ToRangeExt},
    server::{client::Notifier, Result},
    session::DocumentSnapshot,
};

pub(crate) struct PrepareRename;

impl super::RequestHandler for PrepareRename {
    type RequestType = req::PrepareRenameRequest;
}

impl super::BackgroundDocumentRequestHandler for PrepareRename {
    super::define_document_url!(params: &types::TextDocumentPositionParams);

    fn run_with_snapshot(
        snapshot: DocumentSnapshot,
        _notifier: Notifier,
        params: types::TextDocumentPositionParams,
    ) -> Result<Option<types::PrepareRenameResponse>> {
        Ok(prepare_rename(&snapshot, params))
    }
}

fn prepare_rename(
    snapshot: &DocumentSnapshot,
    params: types::TextDocumentPositionParams,
) -> Option<types::PrepareRenameResponse> {
    let current_file_path = params.text_document.uri.to_file_path().ok()?;
    let document = snapshot.document();
    let db = snapshot.db();
    let node_stack = db.indexer().node_stack(&current_file_path);
    let offset = position_to_offset(document.contents(), &params.position, document.index());
    let node = node_at_offset(node_stack.nodes(), offset)?;

    is_node_valid_for_rename(node, offset.into()).map(|range| {
        types::PrepareRenameResponse::Range(range.to_range(
            document.contents(),
            document.index(),
            snapshot.encoding(),
        ))
    })
}

/// Check if the thing that is being targeted for a rename is valid. If it is, return
/// its TextRange.
fn is_node_valid_for_rename(node: &AnyNodeRef, offset: TextSize) -> Option<TextRange> {
    Some(match node {
        AnyNodeRef::NameExpr(_) | AnyNodeRef::Alias(_) | AnyNodeRef::Parameter(_) => node.range(),
        AnyNodeRef::StmtFunctionDef(python_ast::FunctionDefStmt { name, .. })
        | AnyNodeRef::StmtClassDef(python_ast::ClassDefStmt { name, .. })
            if name.range().contains_inclusive(offset) =>
        {
            name.range()
        }
        AnyNodeRef::StmtImportFrom(python_ast::ImportFromStmt {
            module: Some(module),
            ..
        }) if module.range().contains_inclusive(offset) => module.range(),
        AnyNodeRef::AttributeExpr(python_ast::AttributeExpr { attr, .. })
            if attr.range().contains_inclusive(offset) =>
        {
            attr.range()
        }
        AnyNodeRef::ExceptHandlerExceptHandler(python_ast::ExceptHandlerExceptHandler {
            name: Some(name),
            ..
        }) if name.range().contains_inclusive(offset) => name.range(),
        AnyNodeRef::CallExpr(python_ast::CallExpr { arguments, .. }) => {
            return arguments.keywords.iter().find_map(|keyword| {
                if let Some(arg) = &keyword.arg {
                    arg.range().contains_inclusive(offset).then(|| arg.range())
                } else {
                    None
                }
            });
        }
        AnyNodeRef::PatternKeyword(python_ast::PatternKeyword { attr, .. })
            if attr.range().contains_inclusive(offset) =>
        {
            attr.range()
        }
        AnyNodeRef::PatternMatchAs(python_ast::PatternMatchAs {
            name: Some(name), ..
        }) if name.range().contains_inclusive(offset) => name.range(),
        _ => return None,
    })
}
