use std::{path::PathBuf, sync::Arc};

use lsp_types::{self as types, request as req, Url};
use ruff_source_file::LineIndex;
use ruff_text_size::{Ranged, TextRange};
use rustc_hash::FxHashMap;
use sith_python_ast::{
    self as ast,
    visitor::{self, Visitor},
    AnyNodeRef, Expr, Pattern, Stmt, Suite,
};
use sith_python_ast_utils::{
    node_at_offset, node_identifier_at_offset,
    nodes::{NodeStack, NodeWithParent, Nodes},
};
use sith_semantic_model::{
    self as sm,
    db::{FileId, SymbolTableDb},
    declaration::DeclarationQuery,
    symbol_table::SymbolTable,
    type_inference::{PythonType, ResolvedType, TypeInferer},
    ScopeId, ScopeKind,
};
use types::Location;

use crate::{
    edit::{position_to_offset, ToLocation},
    server::{client::Notifier, Result},
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
        Ok(references(&snapshot, params))
    }
}

pub(crate) fn references(
    snapshot: &DocumentSnapshot,
    params: types::ReferenceParams,
) -> Option<Vec<Location>> {
    let current_file_path = Arc::new(snapshot.url().to_file_path().ok()?);

    let db = snapshot.db();
    let document = snapshot.document();

    let index = document.index();
    let position = params.text_document_position.position;
    let offset = position_to_offset(document.contents(), &position, index);
    let ast = db.indexer().ast_or_panic(&current_file_path);
    let node_stack = NodeStack::default().build(ast.suite());

    let symbol_node = node_at_offset(node_stack.nodes(), offset)?;
    let symbol_name = node_identifier_at_offset(symbol_node, offset)?;

    let (scope_id, _) = db.find_enclosing_scope(&current_file_path, offset);

    let references = ReferencesFinder::new(db, &current_file_path).find(
        symbol_name,
        scope_id,
        symbol_node,
        ast.suite(),
        if params.context.include_declaration {
            IncludeDeclaration::Yes
        } else {
            IncludeDeclaration::No
        },
        DoGlobalSearch::Yes,
    );

    if references.is_empty() {
        return None;
    }

    let mut locations = Vec::with_capacity(references.len());
    for (file_id, ranges) in references {
        let path = db.indexer().file_path(&file_id);

        let source = if *path == current_file_path {
            snapshot.document().contents()
        } else {
            // TODO: log if this fails
            &sm::util::read_to_string(path.as_path()).ok()?
        };

        let index = LineIndex::from_source_text(source);

        for range in ranges {
            let url = Url::from_file_path(path.as_path()).ok()?;
            locations.push(range.to_location(url, source, &index, snapshot.encoding()));
        }
    }
    Some(locations)
}

pub(super) enum DoGlobalSearch {
    Yes,
    No,
}

#[derive(Clone, Copy)]
pub(super) enum IncludeDeclaration {
    Yes,
    No,
}

impl IncludeDeclaration {
    fn yes(&self) -> bool {
        matches!(self, Self::Yes)
    }

    fn no(&self) -> bool {
        matches!(self, Self::No)
    }
}

pub(super) struct ReferencesFinder<'db, 'p> {
    db: &'db SymbolTableDb,
    current_file: &'p Arc<PathBuf>,
}

impl<'db, 'p> ReferencesFinder<'db, 'p> {
    pub(super) fn new(db: &'db SymbolTableDb, current_file: &'p Arc<PathBuf>) -> Self {
        Self { db, current_file }
    }

    pub(super) fn find(
        &self,
        symbol_name: &str,
        scope_id: ScopeId,
        symbol_node: &NodeWithParent,
        suite: &Suite,
        include_declaration: IncludeDeclaration,
        mut do_global_search: DoGlobalSearch,
    ) -> FxHashMap<FileId, Vec<TextRange>> {
        let mut result = FxHashMap::default();

        let is_symbol_part_of_attr = matches!(symbol_node.node(), AnyNodeRef::AttributeExpr(_));
        let node_stack = NodeStack::default().build(suite);
        let mut type_inferer = TypeInferer::new(self.db, scope_id, self.current_file.clone());
        let symbol_type = type_inferer.infer_node(symbol_node, node_stack.nodes());

        if symbol_type == ResolvedType::Unknown {
            return FxHashMap::default();
        }
        // This closure determines which files to include in the global reference search.
        // It filters for files that import our target symbol from the current file,
        // ensuring we only search in files that could potentially reference our symbol.
        let mut check_import_source: Box<dyn Fn(&SymbolTable) -> bool> =
            Box::new(|table: &SymbolTable| {
                table
                    .symbol_declaration(symbol_name, ScopeId::global(), DeclarationQuery::Last)
                    .and_then(|decl| decl.import_source()?.any_path())
                    .is_some_and(|import_path| import_path == self.current_file)
            });

        // For attribute expressions (obj.attr), handle the special case differently:
        // 1. First infer the type of the base object ('obj')
        // 2. Update our search strategy to find references based on the origin file of that type
        // 3. Modify the target symbol name to look for the class/module name instead of the attribute
        // This allows us to correctly find references to attributes across imported classes/modules
        if let AnyNodeRef::AttributeExpr(attr) = symbol_node.node() {
            let (origin_file, target_name) =
                match type_inferer.infer_expr(attr.value.as_ref(), node_stack.nodes()) {
                    ResolvedType::KnownType(PythonType::Class(class_type)) => {
                        let origin_file = self.db.indexer().file_path(&class_type.file_id);
                        let class_name = self.db.symbol_name(origin_file, class_type.symbol_id);
                        (origin_file.clone(), class_name.to_string())
                    }
                    ResolvedType::KnownType(PythonType::Module(import_source)) => {
                        let Some(origin_file) = import_source.any_path() else {
                            return FxHashMap::default();
                        };
                        let module_name_py = origin_file.file_name().unwrap().to_string_lossy();
                        let module_name = module_name_py.split('.').nth(0).unwrap();
                        (origin_file.clone(), module_name.to_string())
                    }
                    _ => unreachable!(),
                };

            check_import_source = Box::new(move |table: &SymbolTable| {
                table
                    .symbol_declaration(&target_name, ScopeId::global(), DeclarationQuery::Last)
                    .and_then(|decl| decl.import_source()?.any_path())
                    .is_some_and(|import_path| import_path == &origin_file)
            });
        }

        let current_file_id = self.db.indexer().file_id(self.current_file);
        let table = self.db.table(self.current_file);
        let references_visitor = ReferencesFinderVisitor::new(
            table,
            symbol_name,
            &symbol_type,
            type_inferer,
            node_stack.nodes(),
            include_declaration,
        );
        // Find references in the current file
        result.insert(current_file_id, references_visitor.find_references(suite));

        // TODO: find a way to avoid traversing the entire AST if the symbol was defined locally.
        let scope_kind = table
            .lookup_symbol(symbol_name, scope_id)
            .map(|(_, symbol)| {
                self.db
                    .scope(self.current_file, symbol.definition_scope())
                    .kind()
            });

        // Check if the symbol was defined locally to a scope and skip global search.
        if !is_symbol_part_of_attr
            && matches!(
                scope_kind,
                Some(ScopeKind::Function | ScopeKind::Lambda | ScopeKind::Comprehension)
            )
        {
            do_global_search = DoGlobalSearch::No;
        }

        if matches!(do_global_search, DoGlobalSearch::No) {
            return result;
        }

        // If the symbol we're trying to find the references for is imported we need to
        // search for references in the file where it was declared.
        self.find_references_in_origin(
            symbol_name,
            scope_id,
            &symbol_type,
            is_symbol_part_of_attr,
            include_declaration,
            &mut result,
        );

        for (file_id, table) in self
            .db
            .indexer()
            .tables_iter()
            // skip searching the current file for references
            .filter(|(file_id, _)| **file_id != current_file_id)
            // only search files that import `symbol_name`
            .filter(|(_, table)| check_import_source(table))
        {
            let path = self.db.indexer().file_path(file_id);
            let suite = self.db.indexer().ast_or_panic(path).suite();
            let node_stack = NodeStack::default().build(suite);
            let type_inferer = TypeInferer::new(self.db, ScopeId::global(), path.clone());
            let references_visitor = ReferencesFinderVisitor::new(
                table,
                symbol_name,
                &symbol_type,
                type_inferer,
                node_stack.nodes(),
                include_declaration,
            );
            let references = references_visitor.find_references(suite);
            if !references.is_empty() {
                result.insert(*file_id, references);
            }
        }

        result
    }

    fn find_references_in_origin(
        &self,
        symbol_name: &str,
        scope_id: ScopeId,
        symbol_type: &ResolvedType,
        is_symbol_part_of_attr: bool,
        include_declaration: IncludeDeclaration,
        result: &mut FxHashMap<FileId, Vec<TextRange>>,
    ) {
        let path = if is_symbol_part_of_attr {
            let path = match &symbol_type {
                ResolvedType::KnownType(PythonType::Function { file_id, .. }) => {
                    self.db.indexer().file_path(file_id)
                }
                ResolvedType::KnownType(PythonType::Module(import_source)) => {
                    import_source.any_path().unwrap()
                }
                _ => return,
            };
            path
        } else if let Some(import_source) = self
            .db
            .symbol_declaration(
                self.current_file,
                symbol_name,
                scope_id,
                DeclarationQuery::First,
            )
            .and_then(|declaration| declaration.import_source())
            .filter(|import_source| !import_source.is_unresolved())
        {
            import_source.any_path().unwrap()
        } else {
            return;
        };

        let suite = self.db.indexer().ast_or_panic(path).suite();
        let node_stack = NodeStack::default().build(suite);
        let type_inferer = TypeInferer::new(self.db, ScopeId::global(), path.clone());
        let references_visitor = ReferencesFinderVisitor::new(
            self.db.table(path),
            symbol_name,
            symbol_type,
            type_inferer,
            node_stack.nodes(),
            include_declaration,
        );
        result.insert(
            self.db.indexer().file_id(path),
            references_visitor.find_references(suite),
        );
    }
}

struct ReferencesFinderVisitor<'a, 'table, 'nodes, 'st> {
    table: &'table SymbolTable,
    symbol_name: &'a str,
    symbol_type: &'st ResolvedType,
    type_inferer: TypeInferer<'table>,
    nodes: &'nodes Nodes<'nodes>,
    include_declaration: IncludeDeclaration,
    curr_scope: ScopeId,
    visited_scopes: Vec<ScopeId>,
    references: Vec<TextRange>,
}

impl<'a, 'table, 'nodes, 'st> ReferencesFinderVisitor<'a, 'table, 'nodes, 'st> {
    fn new(
        table: &'table SymbolTable,
        symbol_name: &'a str,
        symbol_type: &'st ResolvedType,
        type_inferer: TypeInferer<'table>,
        nodes: &'nodes Nodes<'nodes>,
        include_declaration: IncludeDeclaration,
    ) -> Self {
        Self {
            table,
            symbol_name,
            symbol_type,
            type_inferer,
            nodes,
            include_declaration,
            curr_scope: ScopeId::global(),
            visited_scopes: vec![ScopeId::global()],
            references: Vec::new(),
        }
    }

    fn find_references(mut self, suite: &'a Suite) -> Vec<TextRange> {
        self.visit_body(suite);
        self.references
    }

    fn visit_next_scope(&mut self) {
        let new_scope = ScopeId::from_usize(self.visited_scopes.len());
        self.visited_scopes.push(new_scope);
        self.curr_scope = new_scope;
        self.type_inferer.set_scope(self.curr_scope);
    }

    fn pop_scope(&mut self) {
        self.curr_scope = self
            .table
            .scope(self.curr_scope)
            .expect("no current scope")
            .parent()
            .expect("attempted to pop without parent scope");
        self.type_inferer.set_scope(self.curr_scope);
    }
}

impl<'a, 'b> Visitor<'b> for ReferencesFinderVisitor<'a, '_, '_, '_>
where
    'b: 'a,
{
    fn visit_stmt(&mut self, stmt: &'b ast::Stmt) {
        match stmt {
            Stmt::FunctionDef(ast::FunctionDefStmt {
                name,
                body,
                parameters,
                decorator_list,
                type_params,
                returns,
                ..
            }) => {
                if self.include_declaration.yes()
                    && self.symbol_name == name.as_str()
                    && self.symbol_type
                        == &self.type_inferer.infer_symbol(
                            name,
                            self.nodes,
                            DeclarationQuery::AtOffset(name.range().start().to_u32()),
                        )
                {
                    self.references.push(name.range());
                }
                for decorator in decorator_list {
                    self.visit_decorator(decorator);
                }
                if let Some(type_params) = type_params {
                    self.visit_type_params(type_params);
                }
                if let Some(returns) = returns {
                    self.visit_expr(returns);
                }
                self.visit_next_scope();
                self.visit_parameters(parameters);
                self.visit_body(body);
                self.pop_scope();
            }
            Stmt::ClassDef(ast::ClassDefStmt {
                name,
                body,
                decorator_list,
                arguments,
                type_params,
                ..
            }) => {
                if self.include_declaration.yes()
                    && self.symbol_name == name.as_str()
                    && self.symbol_type
                        == &self.type_inferer.infer_symbol(
                            name,
                            self.nodes,
                            DeclarationQuery::AtOffset(name.range().start().to_u32()),
                        )
                {
                    self.references.push(name.range());
                }
                for decorator in decorator_list {
                    self.visit_decorator(decorator);
                }
                if let Some(arguments) = arguments {
                    self.visit_arguments(arguments);
                }
                if let Some(type_params) = type_params {
                    self.visit_type_params(type_params);
                }

                self.visit_next_scope();
                self.visit_body(body);
                self.pop_scope();
            }
            _ => visitor::walk_stmt(self, stmt),
        }
    }

    fn visit_expr(&mut self, expr: &'b ast::Expr) {
        match expr {
            Expr::Name(ast::NameExpr { id, ctx, range }) if id == self.symbol_name => {
                if ctx.is_store() && self.include_declaration.no() {
                    return;
                }

                if &self.type_inferer.infer_expr(expr, self.nodes) == self.symbol_type {
                    self.references.push(*range);
                }
            }
            Expr::Attribute(ast::AttributeExpr {
                value, attr, ctx, ..
            }) => {
                self.visit_expr(value);
                if ctx.is_store() && self.include_declaration.no() {
                    return;
                }
                if attr == self.symbol_name
                    && self.symbol_type == &self.type_inferer.infer_expr(expr, self.nodes)
                {
                    self.references.push(attr.range);
                }
            }
            Expr::Lambda(ast::LambdaExpr {
                parameters, body, ..
            }) => {
                self.visit_next_scope();
                if let Some(parameters) = parameters {
                    self.visit_parameters(parameters);
                }
                self.visit_expr(body);
                self.pop_scope();
            }
            Expr::ListComp(ast::ListCompExpr {
                elt, generators, ..
            })
            | Expr::SetComp(ast::SetCompExpr {
                elt, generators, ..
            })
            | Expr::Generator(ast::GeneratorExpr {
                elt, generators, ..
            }) => {
                self.visit_next_scope();
                for comprehension in generators {
                    self.visit_comprehension(comprehension);
                }
                self.visit_expr(elt);
                self.pop_scope();
            }
            Expr::DictComp(ast::DictCompExpr {
                key,
                value,
                generators,
                ..
            }) => {
                self.visit_next_scope();
                for comprehension in generators {
                    self.visit_comprehension(comprehension);
                }
                self.visit_expr(key);
                self.visit_expr(value);
                self.pop_scope();
            }
            _ => visitor::walk_expr(self, expr),
        }
    }

    fn visit_alias(&mut self, alias: &'b ast::Alias) {
        if self.include_declaration.no() {
            return;
        }

        if &alias.name == self.symbol_name
            && self.symbol_type
                == &self
                    .type_inferer
                    .infer_symbol(&alias.name, self.nodes, DeclarationQuery::First)
        {
            self.references.push(alias.range());
        }

        if let Some(alias) = &alias.asname {
            if alias == self.symbol_name
                && self.symbol_type
                    == &self
                        .type_inferer
                        .infer_symbol(alias, self.nodes, DeclarationQuery::First)
            {
                self.references.push(alias.range());
            }
        }
    }

    fn visit_parameter(&mut self, parameter: &'b ast::Parameter) {
        if self.include_declaration.yes()
            && parameter.name.as_str() == self.symbol_name
            && self.symbol_type
                == &self.type_inferer.infer_symbol(
                    &parameter.name,
                    self.nodes,
                    DeclarationQuery::First,
                )
        {
            self.references.push(parameter.name.range());
        }
    }

    fn visit_except_handler(&mut self, except_handler: &'b ast::ExceptHandler) {
        let ast::ExceptHandler::ExceptHandler(ast::ExceptHandlerExceptHandler {
            type_,
            name,
            body,
            ..
        }) = except_handler;

        if let Some(type_) = type_ {
            self.visit_expr(type_);
        }
        if let Some(name) = name {
            if self.include_declaration.yes()
                && name == self.symbol_name
                && self.symbol_type
                    == &self
                        .type_inferer
                        .infer_symbol(name, self.nodes, DeclarationQuery::First)
            {
                self.references.push(name.range());
            }
        }
        self.visit_body(body);
    }

    fn visit_pattern(&mut self, pattern: &'b ast::Pattern) {
        match pattern {
            Pattern::MatchAs(ast::PatternMatchAs { name, pattern, .. }) => {
                if let Some(pattern) = pattern {
                    self.visit_pattern(pattern);
                }
                if let Some(name) = name {
                    if name == self.symbol_name
                        && self.symbol_type
                            == &self.type_inferer.infer_symbol(
                                name,
                                self.nodes,
                                DeclarationQuery::AtOffset(name.range().start().to_u32()),
                            )
                    {
                        self.references.push(name.range());
                    }
                }
            }
            Pattern::MatchSequence(ast::PatternMatchSequence { patterns, .. }) => {
                for pattern in patterns {
                    self.visit_pattern(pattern);
                }
            }
            _ => visitor::walk_pattern(self, pattern),
        }
    }
}
