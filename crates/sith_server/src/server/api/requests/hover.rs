use std::path::PathBuf;

use lsp_types::{self as types, request as req, HoverContents, MarkupContent, MarkupKind};
use python_ast_utils::{node_at_offset, nodes::NodeStack};
use python_utils::nodes::get_documentation_string_from_node;
use semantic_model::type_inference::{PythonType, ResolvedType, TypeInferer};

use crate::{
    edit::position_to_offset,
    server::{api::LSPResult, client::Notifier, Result},
    session::DocumentSnapshot,
};

pub(crate) struct Hover;

impl super::RequestHandler for Hover {
    type RequestType = req::HoverRequest;
}

impl super::BackgroundDocumentRequestHandler for Hover {
    fn document_url(params: &types::HoverParams) -> std::borrow::Cow<lsp_types::Url> {
        std::borrow::Cow::Borrowed(&params.text_document_position_params.text_document.uri)
    }

    fn run_with_snapshot(
        snapshot: DocumentSnapshot,
        _notifier: Notifier,
        params: types::HoverParams,
    ) -> Result<Option<types::Hover>> {
        let document_path = snapshot
            .url()
            .to_file_path()
            .map_err(|_| anyhow::anyhow!("Failed to convert URL to file path"))
            .with_failure_code(lsp_server::ErrorCode::InternalError)?;
        Ok(hover(
            &snapshot,
            &document_path,
            &params.text_document_position_params,
        ))
    }
}

// TODO: show variable type when hovering
pub(crate) fn hover(
    snapshot: &DocumentSnapshot,
    document_path: &PathBuf,
    position: &types::TextDocumentPositionParams,
) -> Option<types::Hover> {
    let db = snapshot.db();
    let ast = db.indexer().ast(document_path).unwrap();
    let node_stack = NodeStack::default().build(ast.suite());
    let nodes = node_stack.nodes();

    let document = snapshot.document();
    let position = position.position;
    let offset = position_to_offset(document.contents(), &position, document.index());

    let (scope_id, _) = db.find_enclosing_scope(document_path, offset);
    let mut type_inferer = TypeInferer::new(db, scope_id, document_path);

    let node = node_at_offset(nodes, offset)?;
    let (file_id, node_id) = match type_inferer.infer_node(node, nodes) {
        ResolvedType::KnownType(PythonType::Class(class_type)) => {
            (class_type.file_id, class_type.node_id)
        }
        ResolvedType::KnownType(PythonType::Function {
            file_id, node_id, ..
        }) => (file_id, node_id),
        _ => return None,
    };
    let path = db.indexer().file_path(&file_id);
    let node_stack = if path.ends_with("stdlib/builtins.pyi") {
        db.builtin_symbols().node_stack()
    } else {
        db.indexer().node_stack(path)
    };
    let doc_str = get_documentation_string_from_node(node_stack.nodes().get(node_id).unwrap())?;

    Some(types::Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: doc_str,
        }),
        range: None,
    })
}
