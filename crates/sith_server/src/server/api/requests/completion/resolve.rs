use std::fs;

use lsp_types::{
    request::{self as req},
    CompletionItem, Documentation, MarkupContent, Url,
};
use python_ast_utils::nodes::NodeStack;
use python_parser::parse_module;
use python_utils::nodes::get_documentation_string_from_node;

use crate::server::api::{
    requests::completion::CompletionItemData,
    traits::{BackgroundDocumentRequestHandler, RequestHandler},
};
use crate::server::Result;

use super::CompletionItemDataPayload;

pub(crate) struct ResolveCompletionItem;
impl RequestHandler for ResolveCompletionItem {
    type RequestType = req::ResolveCompletionItem;
}

impl BackgroundDocumentRequestHandler for ResolveCompletionItem {
    fn document_url(params: &CompletionItem) -> std::borrow::Cow<lsp_types::Url> {
        let data = params.data.as_ref().expect("CompletionItem data");
        let url = Url::from_file_path(
            data["document_uri"]
                .as_str()
                .expect("document_uri to be a string"),
        )
        .expect("string to be a file path");
        std::borrow::Cow::Owned(url)
    }

    // TODO: check completionItem#resolveSupport client capability for properties that can be filled
    // in this request
    // TODO: show documentation for symbols annotated with `Annotated`
    fn run_with_snapshot(
        snapshot: super::DocumentSnapshot,
        _notifier: super::Notifier,
        mut original_completion: CompletionItem,
    ) -> Result<CompletionItem> {
        let data = match original_completion.data.take() {
            Some(it) => it,
            None => return Ok(original_completion),
        };

        let db = snapshot.db();
        let completion_item_data: CompletionItemData =
            serde_json::from_value(data).expect("no error when deserializing!");

        let doc_str = match completion_item_data.payload() {
            Some(CompletionItemDataPayload::Module(completion_item_module_data)) => {
                // Check if the module was already indexed and get the first string expression
                // statement. Otherwise, we need to read the file contents and parse it.
                let ast = if let Some(ast) = db.indexer().ast(completion_item_module_data.path()) {
                    ast
                } else {
                    let contents = fs::read_to_string(completion_item_module_data.path())
                        .expect("file to exist!");
                    &parse_module(&contents)
                };
                ast.suite()
                    .first()
                    .and_then(|stmt| stmt.as_expr_stmt())
                    .and_then(|expr_stmt| expr_stmt.value.as_string_literal_expr())
                    .map(|str_expr| str_expr.value.to_string())
            }
            Some(CompletionItemDataPayload::Symbol(completion_item_symbol_data)) => {
                let path = db
                    .indexer()
                    .file_path(&completion_item_symbol_data.file_id());
                let suite = db.indexer().ast(path).unwrap().suite();
                let node_stack = NodeStack::default().build(suite);
                let completion_item_node = node_stack
                    .nodes()
                    .get(completion_item_symbol_data.node_id())
                    .unwrap();

                get_documentation_string_from_node(completion_item_node)
            }
            None => return Ok(original_completion),
        };

        original_completion.documentation = doc_str.map(|doc| {
            Documentation::MarkupContent(MarkupContent {
                kind: lsp_types::MarkupKind::Markdown,
                value: doc,
            })
        });

        Ok(original_completion)
    }
}
