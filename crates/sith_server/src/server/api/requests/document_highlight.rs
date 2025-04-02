use std::sync::Arc;

use lsp_types::{self as types, request as req};
use sith_python_ast_utils::{node_at_offset, node_identifier_at_offset, nodes::NodeStack};

use crate::{
    edit::{position_to_offset, ToRangeExt},
    server::{api::LSPResult, client::Notifier, Result},
    session::DocumentSnapshot,
};

use super::references::{DoGlobalSearch, IncludeDeclaration, ReferencesFinder};

pub(crate) struct DocumentHighlight;

impl super::RequestHandler for DocumentHighlight {
    type RequestType = req::DocumentHighlightRequest;
}

impl super::BackgroundDocumentRequestHandler for DocumentHighlight {
    fn document_url(params: &types::DocumentHighlightParams) -> std::borrow::Cow<lsp_types::Url> {
        std::borrow::Cow::Borrowed(&params.text_document_position_params.text_document.uri)
    }

    fn run_with_snapshot(
        snapshot: DocumentSnapshot,
        _: Notifier,
        params: types::DocumentHighlightParams,
    ) -> Result<Option<Vec<types::DocumentHighlight>>> {
        let current_file = Arc::new(
            snapshot
                .url()
                .to_file_path()
                .map_err(|_| anyhow::anyhow!("Failed to convert URL to file path"))
                .with_failure_code(lsp_server::ErrorCode::InternalError)?,
        );
        let db = snapshot.db();
        let document = snapshot.document();
        let position = params.text_document_position_params.position;

        let offset = position_to_offset(document.contents(), &position, document.index());
        let (scope_id, _) = db.find_enclosing_scope(&current_file, offset);

        let ast = db.indexer().ast_or_panic(&current_file);
        let node_stack = NodeStack::default().build(ast.suite());

        let Some(symbol_node) = node_at_offset(node_stack.nodes(), offset) else {
            return Ok(None);
        };
        let Some(symbol_name) = node_identifier_at_offset(symbol_node, offset) else {
            return Ok(None);
        };
        let references = ReferencesFinder::new(db, &current_file).find(
            symbol_name,
            scope_id,
            symbol_node,
            ast.suite(),
            IncludeDeclaration::Yes,
            DoGlobalSearch::No,
        );

        let result = references
            .get(&db.indexer().file_id(&current_file))
            .unwrap()
            .iter()
            .map(|range| types::DocumentHighlight {
                range: range.to_range(document.contents(), document.index(), snapshot.encoding()),
                // TODO: implement this
                kind: None,
            })
            .collect::<Vec<_>>();

        if result.is_empty() {
            Ok(None)
        } else {
            Ok(Some(result))
        }
    }
}
