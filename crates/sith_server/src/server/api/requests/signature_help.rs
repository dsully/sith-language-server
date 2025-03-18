use std::sync::Arc;

use compact_str::CompactString;
use lsp_types::{self as types, request as req, Url};
use python_ast::AnyNodeRef;
use python_ast_utils::nodes::{NodeWithParent, Nodes};
use python_ast_utils::{call_expr_identifier, expr_to_str, is_function_overloaded, node_at_offset};
use python_utils::nodes::get_documentation_string_from_node;
use ruff_text_size::Ranged;
use semantic_model::declaration::DeclarationQuery;
use semantic_model::type_inference::{PythonType, ResolvedType, TypeInferer};

use crate::edit::position_to_offset;
use crate::server::api::Result;
use crate::{server::client::Notifier, session::DocumentSnapshot};

pub(crate) struct SignatureHelp;

impl super::RequestHandler for SignatureHelp {
    type RequestType = req::SignatureHelpRequest;
}

impl super::BackgroundDocumentRequestHandler for SignatureHelp {
    fn document_url(params: &types::SignatureHelpParams) -> std::borrow::Cow<Url> {
        std::borrow::Cow::Borrowed(&params.text_document_position_params.text_document.uri)
    }

    fn run_with_snapshot(
        snapshot: DocumentSnapshot,
        _notifier: Notifier,
        params: types::SignatureHelpParams,
    ) -> Result<Option<types::SignatureHelp>> {
        Ok(signature_help(&snapshot, params))
    }
}

fn signature_help(
    snapshot: &DocumentSnapshot,
    params: types::SignatureHelpParams,
) -> Option<types::SignatureHelp> {
    let document_path = Arc::new(snapshot.url().to_file_path().ok()?);
    let document = snapshot.document();
    let index = document.index();
    let db = snapshot.db();

    let lsp_position = params.text_document_position_params.position;
    let offset = position_to_offset(document.contents(), &lsp_position, index);

    let node_stack = db.indexer().node_stack(&document_path);
    let node = node_at_offset(node_stack.nodes(), offset)?;

    let call_expr = get_call_expr_at_offset_in_arguments(offset, node, node_stack.nodes())?;

    let symbol_name = call_expr_identifier(&call_expr.func)?;
    let (scope_id, _) = db.find_enclosing_scope(&document_path, offset);

    let mut type_inferer = TypeInferer::new(db, scope_id, document_path.clone());
    let symbol_type =
        type_inferer.infer_symbol(symbol_name, node_stack.nodes(), DeclarationQuery::Last);
    let (file_id, symbol_id) = match symbol_type {
        ResolvedType::KnownType(PythonType::Class(class_type)) => {
            (class_type.file_id, class_type.symbol_id)
        }
        ResolvedType::KnownType(PythonType::Function {
            file_id, symbol_id, ..
        }) => (file_id, symbol_id),
        _ => return None,
    };

    let symbol_file_path = db.indexer().file_path(&file_id);
    let symbol = db.symbol(symbol_file_path, symbol_id);
    let node_stack2 = db.indexer().node_stack(symbol_file_path);
    let nodes = node_stack2.nodes();

    let args_before_cursor = call_expr
        .arguments
        .arguments_source_order()
        .filter(|arg| arg.range().end().to_u32() <= offset)
        .count() as u32;

    let functions = symbol
        .declarations()
        .iter()
        .map(|decl_id| {
            let declaration = db.declaration(&document_path, *decl_id);
            let func_stmt = nodes
                .get(declaration.node_id)
                .and_then(|node| node.as_stmt_function_def())
                .unwrap();
            *func_stmt
        })
        .collect::<Vec<_>>();
    let is_any_func_overloaded = functions
        .iter()
        .any(|func_stmt| is_function_overloaded(&AnyNodeRef::StmtFunctionDef(func_stmt)));

    let mut best_signature_idx = 0;
    let mut function_signatures = Vec::new();

    if is_any_func_overloaded {
        for (signature_idx, func_stmt) in functions.iter().enumerate() {
            let param_count = func_stmt.parameters.len() as u32;
            let diff = param_count.abs_diff(args_before_cursor);

            if diff == 1 {
                best_signature_idx = signature_idx;
            }

            function_signatures.push(FunctionSignature::from(*func_stmt));
        }
    } else {
        function_signatures.push(FunctionSignature::from(*functions.last().unwrap()));
    }

    Some(types::SignatureHelp {
        signatures: function_signatures
            .into_iter()
            .map(FunctionSignature::into_signature_info)
            .collect(),
        active_signature: Some(best_signature_idx as u32),
        active_parameter: Some(args_before_cursor),
    })
}

fn get_call_expr_at_offset_in_arguments<'a>(
    offset: u32,
    node: &'a NodeWithParent,
    nodes: &'a Nodes,
) -> Option<&'a python_ast::CallExpr> {
    let node = if !matches!(node.node(), AnyNodeRef::CallExpr(_)) && node.parent_id().is_some() {
        node.parent_id()
            .and_then(|parent_id| nodes.get(parent_id))
            .unwrap()
    } else {
        node
    };

    matches!(node.node(), AnyNodeRef::CallExpr(python_ast::CallExpr { arguments, .. })
        if arguments.range.contains(offset.into()))
    .then(|| node.node().as_call_expr())
    .flatten()
    .copied()
}

#[derive(Debug)]
struct FunctionSignature {
    name: CompactString,
    params: Vec<CompactString>,
    return_type: Option<String>,
    docs: Option<String>,
}

impl From<&python_ast::FunctionDefStmt> for FunctionSignature {
    fn from(func_stmt: &python_ast::FunctionDefStmt) -> Self {
        Self {
            name: CompactString::new(func_stmt.name.as_str()),
            docs: get_documentation_string_from_node(&AnyNodeRef::StmtFunctionDef(func_stmt)),
            params: func_stmt
                .parameters
                .iter()
                .map(|param| CompactString::new(param.name()))
                .collect(),
            return_type: func_stmt
                .returns
                .as_ref()
                .map(|returns| expr_to_str(returns)),
        }
    }
}

impl FunctionSignature {
    // TODO: show param types
    fn signature_str(&self) -> String {
        format!(
            "{}({}) -> {}",
            self.name,
            self.params.join(", "),
            self.return_type.as_deref().unwrap_or("None")
        )
    }

    fn into_signature_info(self) -> types::SignatureInformation {
        types::SignatureInformation {
            label: self.signature_str(),
            documentation: self.docs.map(|docs| {
                types::Documentation::MarkupContent(types::MarkupContent {
                    kind: types::MarkupKind::Markdown,
                    value: docs,
                })
            }),
            parameters: Some(
                self.params
                    .into_iter()
                    .map(|param| types::ParameterInformation {
                        label: types::ParameterLabel::Simple(param.to_string()),
                        documentation: None,
                    })
                    .collect(),
            ),
            active_parameter: None,
        }
    }
}
