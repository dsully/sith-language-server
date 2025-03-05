use std::sync::Arc;

use lsp_types::{self as types, request as req, HoverContents, MarkupContent, MarkupKind};
use python_ast::AnyNodeRef;
use python_ast_utils::{
    identifier_from_node, node_at_offset,
    nodes::{NodeId, NodeStack, NodeWithParent, Nodes},
};
use python_utils::{get_python_doc, nodes::get_documentation_string_from_node};
use ruff_text_size::{Ranged, TextSize};
use semantic_model::{
    db::{FileId, SymbolTableDb},
    declaration::DeclarationQuery,
    type_inference::{PythonType, ResolvedType, TypeInferer},
    ScopeId,
};

use crate::{
    edit::position_to_offset,
    server::{client::Notifier, Result},
    session::DocumentSnapshot,
};

use super::completion::resolve::get_module_documentation;

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
        Ok(hover(&snapshot, &params.text_document_position_params))
    }
}

// TODO: show variable type when hovering
pub(crate) fn hover(
    snapshot: &DocumentSnapshot,
    position: &types::TextDocumentPositionParams,
) -> Option<types::Hover> {
    let document_path = Arc::new(snapshot.url().to_file_path().ok()?);
    let db = snapshot.db();
    let node_stack = db.indexer().node_stack(&document_path);
    let nodes = node_stack.nodes();

    let document = snapshot.document();
    let position = position.position;
    let offset = position_to_offset(document.contents(), &position, document.index());

    let (scope_id, _) = db.find_enclosing_scope(&document_path, offset);

    let node = node_at_offset(nodes, offset)?;
    let hover_pos = HoverPosition::from_node(offset.into(), node, node_stack.nodes());

    if !should_show_docs(&hover_pos) {
        return None;
    }

    let identifier = identifier_from_node(node, offset)?;
    let mut type_inferer = TypeInferer::new(db, scope_id, document_path);
    let doc_str = match type_inferer.infer_node(node, nodes) {
        ResolvedType::KnownType(PythonType::Class(class_type)) => get_doc_str(
            db,
            snapshot,
            class_type.file_id,
            class_type.node_id,
            identifier,
        )?,
        ResolvedType::KnownType(PythonType::Function {
            file_id, node_id, ..
        }) => get_doc_str(db, snapshot, file_id, node_id, identifier)?,
        ResolvedType::KnownType(PythonType::Module(import_source)) => {
            get_module_documentation(db, import_source.non_stub_path()?)?
        }
        _ => return None,
    };

    Some(types::Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: doc_str,
        }),
        range: None,
    })
}

fn should_show_docs(hover_pos: &HoverPosition) -> bool {
    matches!(
        hover_pos,
        HoverPosition::Id
            | HoverPosition::Attr
            | HoverPosition::Function
            | HoverPosition::Class
            | HoverPosition::Call
    )
}

fn get_doc_str(
    db: &SymbolTableDb,
    snapshot: &DocumentSnapshot,
    file_id: FileId,
    node_id: NodeId,
    identifier: &str,
) -> Option<String> {
    let path = db.indexer().file_path(&file_id);

    // If the symbol being hovered is builtin we need to execute a python script to get the docs
    // for it.
    if path.ends_with("stdlib/builtins.pyi") {
        get_python_doc(snapshot.client_settings().interpreter()?, identifier).ok()?
    }
    // if the symbol came from a stub file we need to get the documentation from the non-stub
    // file. Usually stub files doesn't contain documentation.
    else if path.extension().is_some_and(|ext| ext == "pyi") {
        get_docs_for_non_stub(db, file_id, node_id, identifier)
    }
    // this case is for normal python files (.py)
    else {
        let node_stack = db.indexer().node_stack(path);
        get_documentation_string_from_node(node_stack.nodes().get(node_id).unwrap())
    }
}

fn get_docs_for_non_stub(
    db: &SymbolTableDb,
    file_id: FileId,
    node_id: NodeId,
    identifier: &str,
) -> Option<String> {
    let non_stub_path = db.indexer().non_stub_path(&file_id)?;
    if db.indexer().is_indexed(non_stub_path) {
        let node_stack = db.indexer().node_stack(non_stub_path);
        get_documentation_string_from_node(node_stack.nodes().get(node_id).unwrap())
    } else {
        let content = std::fs::read_to_string(non_stub_path.as_path()).ok()?;
        let (table, ast) =
            db.indexer()
                .symbol_table_builder(non_stub_path, file_id, true, &content);
        let node_stack = NodeStack::default().is_thirdparty(true).build(ast.suite());

        let declaration =
            table.symbol_declaration(identifier, ScopeId::global(), DeclarationQuery::Last)?;

        get_documentation_string_from_node(node_stack.nodes().get(declaration.node_id).unwrap())
    }
}

#[derive(Debug)]
enum HoverPosition {
    /// Hovering over an identifier (e.g., variable name, function name)
    Id,
    /// Hovering over an assignment statement
    Assignment,
    /// Hovering over a parameter definition
    Param,
    /// Hovering over an attribute access (e.g., `object.attribute`)
    Attr,
    /// Hovering over a function definition
    Function,
    /// Hovering over a class definition
    Class,
    /// Hovering over a function call expression
    Call,
    /// Hovering over other things that we don't care about (e.g. number literals, strings, etc.)
    Other,
}

impl HoverPosition {
    fn from_node(offset: TextSize, node: &NodeWithParent, nodes: &Nodes) -> Self {
        match node.node() {
            AnyNodeRef::NameExpr(_) => {
                if let Some(parent_id) = node.parent_id() {
                    return HoverPosition::from_node(offset, nodes.get(parent_id).unwrap(), nodes);
                }
                Self::Id
            }
            AnyNodeRef::StmtAssign(assign_stmt)
                if assign_stmt
                    .targets
                    .iter()
                    .any(|target| target.range().contains(offset)) =>
            {
                Self::Assignment
            }
            AnyNodeRef::StmtAnnAssign(ann_assign_stmt)
                if ann_assign_stmt.target.range().contains(offset) =>
            {
                Self::Assignment
            }
            AnyNodeRef::StmtAnnAssign(ann_assign_stmt)
                if ann_assign_stmt.annotation.range().contains(offset) =>
            {
                Self::Id
            }
            AnyNodeRef::StmtImportFrom(python_ast::ImportFromStmt {
                module: Some(module),
                ..
            }) if module.range().contains(offset) => Self::Id,
            AnyNodeRef::Alias(_) => Self::Id,
            AnyNodeRef::AttributeExpr(_) => Self::Attr,
            AnyNodeRef::StmtClassDef(_) => Self::Class,
            AnyNodeRef::StmtFunctionDef(_) => Self::Function,
            AnyNodeRef::CallExpr(_) => Self::Call,
            AnyNodeRef::Parameter(_) => Self::Param,
            _ => Self::Other,
        }
    }
}
