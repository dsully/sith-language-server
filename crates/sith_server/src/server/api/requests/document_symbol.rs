use lsp_types::{
    self as types, request as req, DocumentSymbolResponse, Location, SymbolInformation, SymbolKind,
    SymbolTag, Url,
};
use python_ast_utils::is_class_or_function_deprecated;
use ruff_text_size::{Ranged, TextRange};
use rustc_hash::FxHashMap;
use semantic_model::declaration::{DeclStmt, Declaration, DeclarationKind};
use semantic_model::{ScopeId, ScopeKind, Symbol};

use crate::edit::ToRangeExt;
use crate::server::Result;
use crate::{server::client::Notifier, session::DocumentSnapshot};

pub(crate) struct DocumentSymbol;

impl super::RequestHandler for DocumentSymbol {
    type RequestType = req::DocumentSymbolRequest;
}

impl super::BackgroundDocumentRequestHandler for DocumentSymbol {
    super::define_document_url!(params: &types::DocumentSymbolParams);

    fn run_with_snapshot(
        snapshot: DocumentSnapshot,
        _notifier: Notifier,
        params: types::DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        Ok(document_symbol(&snapshot, &params))
    }
}

fn document_symbol(
    snapshot: &DocumentSnapshot,
    params: &types::DocumentSymbolParams,
) -> Option<DocumentSymbolResponse> {
    let document_path = params.text_document.uri.to_file_path().ok()?;
    let db = snapshot.db();
    let node_stack = db.indexer().node_stack(&document_path);

    let mut filtered_symbols = db
        .scopes(&document_path)
        // filter out symbols created in these scopes
        .filter(|scope| !matches!(scope.kind(), ScopeKind::Comprehension | ScopeKind::Lambda))
        .flat_map(|scope| scope.symbols())
        .map(|(name, symbol_id)| {
            let symbol = db.symbol(&document_path, *symbol_id);
            let declaration = db.declaration(&document_path, symbol.declarations().first());
            (name, symbol_id, symbol, declaration)
        })
        // filter out imported symbols and parameters
        .filter(|(_, _, _, declaration)| !declaration.is_import() && !declaration.is_parameter())
        .collect::<Vec<_>>();
    filtered_symbols.sort_by_key(|(_, symbol_id, _, _)| **symbol_id);

    // Map scope IDs defined by class/function symbols to their indices
    let scope_to_symbol_index: FxHashMap<ScopeId, usize> = filtered_symbols
        .iter()
        .enumerate()
        .filter(|(_, (_, _, _, declaration))| {
            matches!(
                declaration.kind,
                DeclarationKind::Stmt(DeclStmt::Class(_) | DeclStmt::Function(_))
            )
        })
        .map(|(i, (_, _, _, declaration))| (declaration.body_scope().unwrap(), i))
        .collect();

    let symbol_nodes: Vec<_> = filtered_symbols
        .into_iter()
        .map(|(name, _, symbol, declaration)| {
            let node = node_stack.get(declaration.node_id).unwrap();
            SymbolNode {
                parent: scope_to_symbol_index
                    .get(&symbol.definition_scope())
                    .copied(),
                label: name.to_string(),
                navigation_range: declaration.range,
                node_range: node.range(),
                kind: SymbolNode::kind(declaration, symbol),
                detail: None,
                deprecated: is_class_or_function_deprecated(node),
            }
        })
        .collect();

    let contents = snapshot.document().contents();
    let index = snapshot.document().index();
    let mut parents = symbol_nodes
        .into_iter()
        .map(|symbol_node| {
            (
                #[allow(deprecated)]
                types::DocumentSymbol {
                    name: symbol_node.label,
                    detail: symbol_node.detail,
                    kind: symbol_node.kind,
                    tags: symbol_node
                        .deprecated
                        .then_some(vec![SymbolTag::DEPRECATED]),
                    deprecated: Some(symbol_node.deprecated),
                    range: symbol_node
                        .node_range
                        .to_range(contents, index, snapshot.encoding()),
                    selection_range: symbol_node.navigation_range.to_range(
                        contents,
                        index,
                        snapshot.encoding(),
                    ),
                    children: None,
                },
                symbol_node.parent,
            )
        })
        .collect::<Vec<_>>();

    // Builds hierarchy from a flat list, in reverse order (so that indices
    // makes sense)
    let document_symbols = {
        let mut acc = Vec::new();
        while let Some((mut node, parent_idx)) = parents.pop() {
            if let Some(children) = &mut node.children {
                children.reverse();
            }
            let parent = match parent_idx {
                None => &mut acc,
                Some(i) => parents[i].0.children.get_or_insert_with(Vec::new),
            };
            parent.push(node);
        }
        acc.reverse();
        acc
    };

    if snapshot.resolved_client_capabilities().hierarchical_symbols {
        Some(document_symbols.into())
    } else {
        let mut symbol_information = Vec::new();
        for symbol in document_symbols {
            flatten_document_symbol(
                &symbol,
                None,
                &params.text_document.uri,
                &mut symbol_information,
            );
        }

        Some(symbol_information.into())
    }
}

#[derive(Debug)]
struct SymbolNode {
    parent: Option<usize>,
    label: String,
    navigation_range: TextRange,
    node_range: TextRange,
    kind: types::SymbolKind,
    detail: Option<String>,
    deprecated: bool,
}

impl SymbolNode {
    fn kind(declaration: &Declaration, symbol: &Symbol) -> SymbolKind {
        match &declaration.kind {
            DeclarationKind::Stmt(DeclStmt::Class(_)) => SymbolKind::CLASS,
            DeclarationKind::Stmt(DeclStmt::Function(_)) => SymbolKind::FUNCTION,
            DeclarationKind::Stmt(DeclStmt::Method(_, _)) if symbol.is_constructor() => {
                SymbolKind::CONSTRUCTOR
            }
            DeclarationKind::Stmt(DeclStmt::Method(_, _)) => SymbolKind::METHOD,
            DeclarationKind::Stmt(DeclStmt::Assignment | DeclStmt::AnnAssign)
                if symbol.is_constant() =>
            {
                SymbolKind::CONSTANT
            }
            DeclarationKind::Stmt(DeclStmt::Assignment | DeclStmt::AnnAssign)
                if symbol.is_class_field() =>
            {
                SymbolKind::FIELD
            }
            DeclarationKind::Stmt(DeclStmt::Assignment | DeclStmt::AnnAssign) => {
                SymbolKind::VARIABLE
            }
            _ => SymbolKind::VARIABLE,
        }
    }
}

fn flatten_document_symbol(
    symbol: &types::DocumentSymbol,
    container_name: Option<String>,
    url: &Url,
    res: &mut Vec<SymbolInformation>,
) {
    #[allow(deprecated)]
    res.push(SymbolInformation {
        name: symbol.name.clone(),
        kind: symbol.kind,
        tags: symbol
            .deprecated
            .and_then(|deprecated| deprecated.then_some(vec![SymbolTag::DEPRECATED])),
        deprecated: symbol.deprecated,
        location: Location::new(url.clone(), symbol.range),
        container_name,
    });

    for child in symbol.children.iter().flatten() {
        flatten_document_symbol(child, Some(symbol.name.clone()), url, res);
    }
}
