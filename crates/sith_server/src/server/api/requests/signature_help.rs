use std::sync::Arc;

use compact_str::CompactString;
use lsp_types::{self as types, request as req, Url};
use ruff_text_size::{Ranged, TextSize};
use sith_python_ast::{
    self as ast, AnyNodeRef, AnyParameterRef, ArgOrKeyword, Arguments, CallExpr, Parameters,
};
use sith_python_ast_utils::nodes::{NodeWithParent, Nodes};
use sith_python_ast_utils::{
    expr_to_str, is_function_overloaded, node_at_offset, parameter_with_default_to_str,
};
use sith_python_utils::nodes::get_documentation_string_from_node;
use sith_semantic_model::declaration::Declaration;
use sith_semantic_model::type_inference::{PythonType, ResolvedType, TypeInferer};

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
    let (scope_id, _) = db.find_enclosing_scope(&document_path, offset);

    let mut type_inferer = TypeInferer::new(db, scope_id, document_path.clone());
    let symbol_type = type_inferer.infer_expr(call_expr.func.as_ref(), node_stack.nodes());
    let (file_id, symbol_id) = match symbol_type {
        ResolvedType::KnownType(PythonType::Class(class_type)) => {
            let (constructor_symbol_id, _) = class_type.lookup(db, "__init__")?;
            (class_type.file_id, constructor_symbol_id)
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

    let arg_pos = find_arg_position(call_expr, offset.into());
    let functions = symbol
        .declarations()
        .iter()
        .map(|decl_id| {
            let declaration = db.declaration(symbol_file_path, *decl_id);
            let func_stmt = nodes
                .get(declaration.node_id)
                .and_then(|node| node.as_stmt_function_def())
                .unwrap();
            (*func_stmt, declaration)
        })
        .collect::<Vec<_>>();
    let is_any_func_overloaded = functions
        .iter()
        .any(|(func_stmt, _)| is_function_overloaded(&AnyNodeRef::StmtFunctionDef(func_stmt)));

    let mut active_parameter = None;
    let mut best_signature_idx = None;
    let mut function_signatures = Vec::new();

    if is_any_func_overloaded {
        for (signature_idx, (func_stmt, declaration)) in functions.iter().enumerate() {
            if let Some(param_idx) = find_active_parameter(
                arg_pos,
                declaration,
                &func_stmt.parameters,
                &call_expr.arguments,
            ) {
                let param_count = func_stmt.parameters.len() as u32;
                if param_count.abs_diff(param_idx) == 1 {
                    best_signature_idx = Some(signature_idx as u32);
                }
                active_parameter = Some(param_idx);
            }

            function_signatures.push(FunctionSignature::from_function(func_stmt));
        }
    } else {
        // get the correct function declaration based on the offset
        let declaration = symbol
            .declarations()
            .at_offset(db.table(symbol_file_path), offset)?;

        let func_stmt = nodes
            .get(declaration.node_id)
            .and_then(|node| node.as_stmt_function_def())
            .unwrap();
        active_parameter = find_active_parameter(
            arg_pos,
            declaration,
            &func_stmt.parameters,
            &call_expr.arguments,
        );
        function_signatures.push(FunctionSignature::from_function(func_stmt));
    }

    Some(types::SignatureHelp {
        signatures: function_signatures
            .into_iter()
            .map(FunctionSignature::into_signature_info)
            .collect(),
        active_signature: best_signature_idx,
        active_parameter,
    })
}

fn get_call_expr_at_offset_in_arguments<'a>(
    offset: u32,
    node: &'a NodeWithParent,
    nodes: &'a Nodes,
) -> Option<&'a ast::CallExpr> {
    let mut node = node;
    // If the node is not a call expression, walk through it's parents until it has no parent
    // or it is a call expression.
    while !matches!(node.node(), AnyNodeRef::CallExpr(_)) && node.parent_id().is_some() {
        node = node
            .parent_id()
            .and_then(|parent_id| nodes.get(parent_id))
            .unwrap();
    }

    matches!(node.node(), AnyNodeRef::CallExpr(ast::CallExpr { arguments, .. })
        if arguments.range.contains_inclusive(offset.into()))
    .then(|| node.node().as_call_expr())
    .flatten()
    .copied()
}

fn find_arg_position(call: &CallExpr, offset: TextSize) -> CursorPosition {
    // TODO: Is it possible to remove this allocation here?
    let arguments: Vec<_> = call.arguments.arguments_source_order().collect();
    arguments
        .iter()
        .enumerate()
        .rev()
        .find_map(|(idx, arg)| {
            if arg.range().contains_inclusive(offset) {
                if let ArgOrKeyword::Keyword(keyword) = arg {
                    Some(CursorPosition::InKeyword(idx, keyword.arg.as_deref()))
                } else {
                    Some(CursorPosition::InArg(idx))
                }
            } else if offset > arg.start() {
                if let ArgOrKeyword::Keyword(keyword) = arg {
                    Some(CursorPosition::AfterKeyword(idx, keyword.arg.as_deref()))
                } else {
                    Some(CursorPosition::AfterArg(idx))
                }
            } else {
                None
            }
        })
        .unwrap_or(CursorPosition::Empty)
}

// FIX: In the following case:
// def foo(p1, p2, /, p_or_k1, *args, kw1, kw2, **kwargs): pass
// foo(1, 2, 3, 4, 5, 6, kw1=1, <cursor>)
//
// `**kwargs` is highlighted in the function signature instead of `kw2`.
// This is due the presence of `*args`, the index we get from the call site is
// bigger than the number of parameters. We need a way of computing the amount of
// arguments that belongs to `*args` and fix the index.
fn find_active_parameter(
    arg_pos: CursorPosition,
    declaration: &Declaration,
    parameters: &Parameters,
    arguments: &Arguments,
) -> Option<u32> {
    let self_offset = if declaration.is_method() { 1 } else { 0 };

    let (param_idx, arg_idx) = match arg_pos {
        CursorPosition::InArg(arg_idx) => (arg_idx + self_offset, arg_idx),
        CursorPosition::AfterArg(arg_idx) => (arg_idx + self_offset + 1, arg_idx),
        CursorPosition::AfterKeyword(arg_idx, name) => {
            if let Some(name) = name {
                // Validate that the keyword exists in the signature
                if parameters.find(name).is_none() {
                    return Some(u32::MAX); // Invalid keyword, clear highlight
                }
            }
            // Suggest the next parameter based on argument position
            (arg_idx + self_offset + 1, arg_idx)
        }
        // Cursor is inside a keyword argument at index `arg_idx` with optional name
        CursorPosition::InKeyword(arg_idx, name) => {
            if let Some(name) = name {
                if let Some((idx, _)) = parameters.find(name) {
                    // Use the parameter index corresponding to the keyword
                    (idx, arg_idx)
                } else if parameters.kwarg.is_some() {
                    // If keyword isn't found but **kwargs exists, point to it
                    let mut idx = parameters.posonlyargs.len()
                        + parameters.args.len()
                        + usize::from(parameters.vararg.is_some())
                        + parameters.kwonlyargs.len();
                    if !parameters.posonlyargs.is_empty() {
                        idx += 1; // Account for '/' separator
                    }
                    if !parameters.kwonlyargs.is_empty() {
                        idx += 1; // Account for '*' separator
                    }
                    (idx, arg_idx)
                } else {
                    return Some(u32::MAX); // Invalid keyword and no **kwargs, clear highlight
                }
            } else {
                (arg_idx, arg_idx)
            }
        }
        CursorPosition::Empty => return None,
    };

    let mut display_idx = param_idx;
    // Get the correspondent parameter for the argument index
    let param = parameters
        .iter()
        .nth(param_idx)
        .or_else(|| parameters.iter().last())
        .unwrap();
    match param {
        AnyParameterRef::KwArgs(_) => {
            let is_after_keyword = matches!(
                arg_pos,
                CursorPosition::AfterArg(id) | CursorPosition::AfterKeyword(id, _)
                if arguments.arguments_source_order().nth(id).is_some_and(|arg| matches!(arg, ArgOrKeyword::Keyword(_)))
            );
            // Check if cursor is in an argument following a keyword argument
            let is_in_arg_after_keyword = matches!(
                arg_pos,
                CursorPosition::InArg(id)
                if id > 0 && arguments.arguments_source_order().nth(id - 1).is_some_and(|arg| matches!(arg, ArgOrKeyword::Keyword(_)))
            );

            if is_after_keyword || is_in_arg_after_keyword {
                display_idx = parameters.posonlyargs.len()
                        + 1 // '/' character
                        + parameters.args.len()
                        + usize::from(parameters.vararg.is_some())
                        + parameters.kwonlyargs.len()
                        + 1  /* '*' character */;
            } else if !matches!(arg_pos, CursorPosition::InKeyword(_, _)) {
                // Point to *args if not in a keyword context
                display_idx = parameters.posonlyargs.len() + parameters.args.len();
                if !parameters.posonlyargs.is_empty() {
                    display_idx += 1; // '/' separator
                }
            }
        }
        AnyParameterRef::Param(_) => {
            // Don't hightlight the `/` character if position only parameters were defined
            if !parameters.posonlyargs.is_empty() {
                display_idx += 1;
            }
        }
        AnyParameterRef::KwOnly(_) => {
            // If there's a vararg before a keyword only parameter, keep the index in the vararg
            // until we see a keyword argument.
            if parameters.vararg.is_some() {
                let mut vararg_idx = parameters.posonlyargs.len() + parameters.args.len();
                let arg = arguments
                    .arguments_source_order()
                    .nth(arg_idx)
                    .or_else(|| arguments.arguments_source_order().last())
                    .unwrap();
                if arg_idx >= vararg_idx && matches!(arg, ArgOrKeyword::Arg(_)) {
                    // Skip the `/` character if position only parameters were defined
                    if !parameters.posonlyargs.is_empty() {
                        vararg_idx += 1;
                    }
                    return Some(vararg_idx as u32);
                }
            }
            // Skip the `/` character if position only parameters were defined
            if !parameters.posonlyargs.is_empty() {
                display_idx += 1;
            }
            // Don't hightlight the `*` character if keyword only parameters were defined
            display_idx += 1;
        }
        AnyParameterRef::VarArgs(_) => {
            // Get the index of *vararg*
            display_idx = parameters.posonlyargs.len() + parameters.args.len();
            // Skip the `/` character if position only parameters were defined
            if !parameters.posonlyargs.is_empty() {
                display_idx += 1;
            }
        }
        _ => {
            let id = if let CursorPosition::AfterArg(id) = arg_pos {
                id
            } else {
                display_idx
            };

            let vararg_idx = parameters.posonlyargs.len() + parameters.args.len();
            if arguments
                .arguments_source_order()
                .nth(id)
                .is_some_and(|arg| matches!(arg, ArgOrKeyword::Arg(_)))
                && id > vararg_idx
            {
                display_idx = vararg_idx + 1;
            }
        }
    }

    Some(display_idx as u32)
}

#[derive(Debug, Clone, Copy)]
enum CursorPosition<'a> {
    /// Text cursor is in a positional argument.
    InArg(usize),
    /// Text cursor is in a keyword argument.
    InKeyword(usize, Option<&'a str>),
    /// Text cursor is after a positional argument.
    AfterArg(usize),
    /// Text cursor is after a keyword argument.
    AfterKeyword(usize, Option<&'a str>),
    /// Text cursor is in an empty function call.
    Empty,
}

#[derive(Debug)]
struct FunctionSignature {
    name: CompactString,
    params: Vec<CompactString>,
    return_type: Option<String>,
    docs: Option<String>,
}

impl FunctionSignature {
    fn from_function(func_stmt: &ast::FunctionDefStmt) -> Self {
        Self {
            name: CompactString::new(func_stmt.name.as_str()),
            docs: get_documentation_string_from_node(&AnyNodeRef::StmtFunctionDef(func_stmt)),
            params: Self::params_signature(&func_stmt.parameters),
            return_type: func_stmt
                .returns
                .as_ref()
                .map(|returns| expr_to_str(returns)),
        }
    }

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

    fn params_signature(params: &Parameters) -> Vec<CompactString> {
        let mut result = Vec::new();

        for posonlyarg in params.posonlyargs.iter() {
            result.push(parameter_with_default_to_str(posonlyarg));
        }

        if !params.posonlyargs.is_empty() {
            result.push(CompactString::new("/"));
        }

        for arg in params.args.iter() {
            result.push(parameter_with_default_to_str(arg));
        }

        if let Some(vararg) = &params.vararg {
            result.push(CompactString::new(format!("*{}", &vararg.name)));
        }

        if !params.kwonlyargs.is_empty() {
            result.push(CompactString::new("*"));
        }

        for kwonlyarg in params.kwonlyargs.iter() {
            result.push(parameter_with_default_to_str(kwonlyarg));
        }

        if let Some(kwarg) = &params.kwarg {
            result.push(CompactString::new(format!("**{}", &kwarg.name)));
        }

        result
    }
}
