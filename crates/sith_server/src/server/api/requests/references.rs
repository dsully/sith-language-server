use std::fs;

use lsp_types::{self as types, request as req, Url};
use python_ast_utils::{identifier_from_node, node_at_offset, nodes::NodeStack};
use ruff_source_file::LineIndex;
use types::Location;

use crate::{
    edit::{position_to_offset, ToLocation},
    server::{api::LSPResult, client::Notifier, Result},
    session::DocumentSnapshot,
};

pub(crate) struct References;

impl super::RequestHandler for References {
    type RequestType = req::References;
}

impl super::BackgroundDocumentRequestHandler for References {
    fn document_url(params: &types::ReferenceParams) -> std::borrow::Cow<lsp_types::Url> {
        std::borrow::Cow::Borrowed(&params.text_document_position.text_document.uri)
    }

    fn run_with_snapshot(
        snapshot: DocumentSnapshot,
        _notifier: Notifier,
        params: types::ReferenceParams,
    ) -> Result<Option<Vec<Location>>> {
        let current_file_path = snapshot
            .url()
            .to_file_path()
            .map_err(|_| anyhow::anyhow!("Failed to convert URI to file path"))
            .with_failure_code(lsp_server::ErrorCode::InternalError)?;

        let db = snapshot.db();
        let document = snapshot.document();

        let index = document.index();
        let position = params.text_document_position.position;
        let offset = position_to_offset(document.contents(), &position, index);
        let ast = db.indexer().ast(&current_file_path).unwrap();
        let node_stack = NodeStack::default().build(ast.suite());

        let Some(symbol_name) = node_at_offset(node_stack.nodes(), offset)
            .and_then(|node_with_parent| identifier_from_node(&node_with_parent.node, offset))
        else {
            return Ok(None);
        };

        let Some(references) = db.references(&current_file_path, symbol_name, offset) else {
            return Ok(None);
        };
        let mut locations = Vec::with_capacity(references.len());
        for (file_id, symbol_occurences) in references {
            let path = db.indexer().file_path(&file_id);

            let source = if *path == current_file_path {
                snapshot.document().contents()
            } else {
                &fs::read_to_string(path)
                    .map_err(|e| anyhow::anyhow!("Failed to read {} contents: {e}", path.display()))
                    .with_failure_code(lsp_server::ErrorCode::RequestFailed)?
            };

            let index = LineIndex::from_source_text(source);

            for symbol_occurrence in symbol_occurences {
                if !params.context.include_declaration && symbol_occurrence.is_declaration() {
                    continue;
                }
                let range = symbol_occurrence.range();
                let url = Url::from_file_path(path)
                    .map_err(|_| anyhow::anyhow!("Failed to convert file path to URL"))
                    .with_failure_code(lsp_server::ErrorCode::InternalError)?;

                locations.push(range.to_location(url, source, &index, snapshot.encoding()));
            }
        }

        Ok(Some(locations))
    }
}
