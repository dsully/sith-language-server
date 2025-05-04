use std::{
    path::Path,
    sync::{Arc, RwLock},
};

use bitflags::bitflags;
use ruff_python_resolver::{
    cache::ImportResolverCache, config::Config, execution_environment::ExecutionEnvironment,
    import_result::ImportType, resolver::resolve_import,
};
use ruff_text_size::{Ranged, TextRange};
use rustc_hash::FxHashMap;
use sith_python_ast::{
    self as ast,
    visitor::{self, Visitor},
    AnyNodeRef, AnyParameterRef, ContextExpr, Expr, Pattern, Stmt, TypeParam,
};
use sith_python_ast_utils::{
    create_import_module_descriptor,
    nodes::{NodeId, Nodes},
};
use sith_python_utils::{is_python_module, PythonHost};

use crate::{
    declaration::{
        DeclId, DeclStmt, Declaration, DeclarationKind, DeclarationQuery, Declarations,
        ImportSource, SymbolDeclarations,
    },
    indexer::FileId,
    symbol::{Symbol, SymbolFlags, SymbolId, Symbols},
    Scope, ScopeId, ScopeKind, Scopes,
};

#[derive(Debug, Clone)]
pub struct SymbolTable {
    pub decls: Declarations,
    pub symbols: Symbols,
    /// Stack of all the scopes in the file.
    pub scopes: Scopes,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            symbols: Symbols::default(),
            decls: Declarations::default(),
            scopes: Scopes::default(),
        }
    }

    pub fn scope(&self, scope_id: ScopeId) -> Option<&Scope> {
        self.scopes.get(scope_id)
    }

    pub fn symbol(&self, symbol_id: SymbolId) -> Option<&Symbol> {
        self.symbols.get(symbol_id)
    }

    pub fn declaration(&self, decl_id: DeclId) -> Option<&Declaration> {
        self.decls.get(decl_id)
    }

    pub fn declarations(&self) -> impl Iterator<Item = &Declaration> {
        self.decls.iter()
    }

    pub fn lookup_symbol(&self, name: &str, scope: ScopeId) -> Option<(SymbolId, &Symbol)> {
        let scope = self.scopes.get(scope).unwrap();
        if let Some(symbol_id) = scope.symbol_id(name) {
            return self.symbol(symbol_id).map(|s| (symbol_id, s));
        }

        // If the symbol is not found in the given `scope` search in the enclosing scopes until it
        // reaches the global scope.
        let parent_id = scope.parent()?;
        self.scopes
            .ancestors(parent_id)
            .find_map(|scope| scope.symbol_id(name))
            .and_then(|symbol_id| self.symbol(symbol_id).map(|s| (symbol_id, s)))
    }

    pub fn find_enclosing_scope(&self, offset: u32) -> (ScopeId, &Scope) {
        self.scopes
            .reversed_scopes()
            .find(|(_, scope)| scope.range().contains_inclusive(offset.into()))
            .unwrap_or((ScopeId::global(), self.scopes.global()))
    }

    pub fn symbol_declaration(
        &self,
        name: &str,
        scope_id: ScopeId,
        query: DeclarationQuery,
    ) -> Option<&Declaration> {
        self.lookup_symbol(name, scope_id).and_then(|(_, symbol)| {
            let mut declaration = match query {
                DeclarationQuery::Last => self.declaration(symbol.declarations().last()),
                DeclarationQuery::First => self.declaration(symbol.declarations().first()),
                DeclarationQuery::AtOffset(offset) => symbol.declarations().at_offset(self, offset),
            };

            if let Some(DeclarationKind::Stmt(DeclStmt::ImportAlias(decl_id))) =
                declaration.map(|decl| &decl.kind)
            {
                declaration = self.declaration(*decl_id);
            }

            declaration
        })
    }
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
}

struct FileInfo<'a> {
    path: &'a Path,
    is_builtin_stub_file: bool,
}

bitflags! {
    #[derive(Debug)]
    struct VisitorFlags: u8 {
        const IN_CLASS = 1 << 0;
        const IN_FUNCTION = 1 << 1;
        const IN_FIRST_PARAM = 1 << 2;
        const IN_ATTRIBUTE_ASSIGNMENT = 1 << 3;

        const IN_INSTANCE_ATTR_ASSIGNMENT = 1 << 4;
    }
}

#[derive(Clone, Copy)]
pub struct ImportResolverConfig<'a> {
    exec_env: &'a ExecutionEnvironment,
    config: &'a Config,
    host: &'a PythonHost,
}

impl<'a> ImportResolverConfig<'a> {
    pub fn new(
        exec_env: &'a ExecutionEnvironment,
        config: &'a Config,
        host: &'a PythonHost,
    ) -> Self {
        Self {
            exec_env,
            config,
            host,
        }
    }
}

// TODO: handle `nonlocal`, `global` and *variable shadowing*
pub struct SymbolTableBuilder<'a> {
    file_info: FileInfo<'a>,
    file_id: FileId,

    curr_scope: ScopeId,
    curr_node: Option<NodeId>,
    curr_declaration_node: Option<NodeId>,
    // Used to handle the instance parameter of a method
    last_visited_class_symbol: Option<SymbolId>,
    flags: VisitorFlags,

    nodes: Nodes<'a>,
    table: SymbolTable,

    import_resolver_cfg: ImportResolverConfig<'a>,
    import_resolver_cache: Arc<RwLock<ImportResolverCache>>,
}

impl<'a> SymbolTableBuilder<'a> {
    pub fn new(
        filepath: &'a Path,
        file_id: FileId,
        import_resolver_cfg: ImportResolverConfig<'a>,
        import_resolver_cache: Arc<RwLock<ImportResolverCache>>,
    ) -> Self {
        Self {
            file_info: FileInfo {
                path: filepath,
                is_builtin_stub_file: filepath.ends_with("stdlib/builtins.pyi"),
            },
            file_id,
            curr_scope: ScopeId::global(),
            curr_node: None,
            curr_declaration_node: None,
            last_visited_class_symbol: None,
            nodes: Nodes::default(),
            table: SymbolTable::new(),
            flags: VisitorFlags::empty(),
            import_resolver_cfg,
            import_resolver_cache,
        }
    }

    pub fn build(mut self, ast: &'a [Stmt]) -> SymbolTable {
        self.visit_body(ast);
        self.table
    }

    fn set_declaration_node(&mut self, node_id: NodeId) {
        self.curr_declaration_node = Some(node_id);
    }

    fn clear_declaration_node(&mut self) {
        self.curr_declaration_node = None;
    }

    fn push_declaration(
        &mut self,
        name: &str,
        kind: DeclarationKind,
        range: TextRange,
        node_id: NodeId,
    ) -> (DeclId, SymbolId) {
        // Handle instance attribute assignments (e.g., `self.foo = 1` inside a method)
        // When assigning to instance attributes:
        // - The attribute needs to be added to the class scope, not the method scope
        // - We identify this case by checking if we're in an instance attribute assignment
        // - The parent scope (class scope) is retrieved to store the attribute
        // Otherwise, use the current scope for regular variable declarations
        let is_instance_attr_assign = self
            .flags
            .contains(VisitorFlags::IN_INSTANCE_ATTR_ASSIGNMENT);

        let scope = if is_instance_attr_assign {
            self.flags.remove(VisitorFlags::IN_INSTANCE_ATTR_ASSIGNMENT);
            self.table
                .scope(self.curr_scope)
                .and_then(|scope| scope.parent())
                .and_then(|parent_id| self.table.scopes.get_mut(parent_id))
                .expect("Couldn't find scope for parent id!")
        } else {
            self.table
                .scopes
                .get_mut(self.curr_scope)
                .expect("Scope does not exist for provided scope id!")
        };

        // don't create multiple declarations for the same import segment
        if let Some(symbol_id) = scope.symbol_id(name) {
            let symbol = self.table.symbols.get(symbol_id).unwrap();
            let import_decl_id = symbol.declarations().last();
            let declaration = self.table.decls.get(import_decl_id).unwrap();
            if matches!(
                declaration.kind,
                DeclarationKind::Stmt(
                    DeclStmt::Import { .. }
                        | DeclStmt::ImportSegment { .. }
                        | DeclStmt::ImportAlias(_)
                )
            ) {
                let decl_id = self.table.decls.insert(
                    self.file_id,
                    DeclarationKind::Stmt(DeclStmt::SameImport(import_decl_id)),
                    node_id,
                    range,
                );
                let declaration = self.table.decls.get_mut(decl_id).unwrap();
                declaration.symbol_id = symbol_id;
                return (decl_id, symbol_id);
            }
        }

        let decl_id = self.table.decls.insert(self.file_id, kind, node_id, range);
        let symbol_id = if let Some(symbol_id) = scope.symbol_id(name) {
            let symbol = self.table.symbols.get_mut(symbol_id).unwrap();
            symbol.push_declaration_id(decl_id);
            symbol_id
        } else {
            let declaration = self.table.decls.get(decl_id).unwrap();
            let mut symbol = Symbol::new(self.curr_scope, SymbolDeclarations::single(decl_id));

            if self.file_info.is_builtin_stub_file {
                symbol.set_flag(SymbolFlags::BUILTIN);
            }
            if name.starts_with("_") {
                symbol.set_flag(SymbolFlags::PRIVATE);
            }
            if name.chars().all(|c| c.is_uppercase()) {
                symbol.set_flag(SymbolFlags::CONSTANT)
            }
            if self.flags.contains(VisitorFlags::IN_CLASS) && name == "__init__" {
                symbol.set_flag(SymbolFlags::CONTRUCTOR);
            }
            if is_instance_attr_assign {
                symbol.set_flag(SymbolFlags::CLASS_FIELD);
            }
            if let Some(path) = declaration.import_source().and_then(ImportSource::any_path) {
                if is_python_module(name, path) {
                    symbol.set_flag(SymbolFlags::MODULE);
                }
            }

            let symbol_id = self.table.symbols.insert(symbol);
            scope.add_symbol(name, symbol_id);
            symbol_id
        };

        let declaration = self.table.decls.get_mut(decl_id).unwrap();
        declaration.symbol_id = symbol_id;

        (decl_id, symbol_id)
    }

    fn push_scope(&mut self, kind: ScopeKind, range: TextRange) -> ScopeId {
        self.table.scopes.insert(Scope::new(
            kind,
            Some(self.curr_scope),
            FxHashMap::default(),
            range,
        ))
    }

    fn pop_scope(&mut self) {
        self.curr_scope = self
            .table
            .scopes
            .get(self.curr_scope)
            .unwrap()
            .parent()
            .unwrap_or_else(|| {
                panic!(
                    "attempted to pop without scope in file `{}` on node: {:#?}",
                    self.file_info.path.display(),
                    self.curr_node.and_then(|node_id| self.nodes.get(node_id))
                )
            });
    }

    fn push_node(&mut self, node: impl Into<AnyNodeRef<'a>>) -> NodeId {
        let node_id = self.nodes.insert(node.into(), self.curr_node);
        self.curr_node = Some(node_id);
        node_id
    }

    fn pop_node(&mut self) {
        let node_id = self.curr_node.expect("Attempt to pop without node");
        self.curr_node = self.nodes.parend_id(node_id);
    }

    fn handle_context(&mut self, ctx: &ContextExpr, name: &str, range: &TextRange) {
        if ctx == &ContextExpr::Store {
            let Some(declaration_node) = self.curr_declaration_node else {
                unreachable!("declaration node wasn't set for `{name}` at {range:?}")
            };
            let node_with_parent = self.nodes.get(declaration_node).unwrap();
            let kind = match node_with_parent.node() {
                AnyNodeRef::StmtFor(_) => DeclarationKind::For,
                AnyNodeRef::NamedExpr(_) => DeclarationKind::Named,
                AnyNodeRef::Comprehension(_) => DeclarationKind::For,
                AnyNodeRef::WithItem(_) => DeclarationKind::WithItem,
                AnyNodeRef::StmtAssign(_) => DeclarationKind::Stmt(DeclStmt::Assignment),
                AnyNodeRef::StmtAugAssign(_) => DeclarationKind::Stmt(DeclStmt::AugAssign),
                AnyNodeRef::StmtAnnAssign(_) => DeclarationKind::Stmt(DeclStmt::AnnAssign),
                AnyNodeRef::StmtTypeAlias(_) => DeclarationKind::Stmt(DeclStmt::TypeAlias),
                // These prevent the server from crashing if the pattern match has invalid syntax
                AnyNodeRef::PatternMatchMapping(_) | AnyNodeRef::PatternMatchValue(_) => {
                    DeclarationKind::PatternMatch
                }
                _ => unreachable!(
                    "declaration node not handled: {:?}\n{:#?}",
                    self.file_info.path,
                    node_with_parent.node()
                ),
            };

            self.push_declaration(name, kind, *range, declaration_node);
        }
    }
}

// FIX: currently the import resolution algorithm doesn't return any path for an import
// where one the segments doesn't exists, e.g., `from foo.baz.foobar import F`
// if `baz` doesn't exist it won't return the path for `foo`. Even though it successfully manages
// to find the path of `foo`.
// TODO: make the import resolver return a `Vec` of `Option`s for the resolved paths; `Some` if the
// path is found and `None` otherwise.

// NOTE: the nodes pushed here should be synced with the `NodeStack` in
// `crates/sith_python_ast_utils/src/nodes.rs`
impl<'a, 'b> Visitor<'b> for SymbolTableBuilder<'a>
where
    'b: 'a,
{
    fn visit_stmt(&mut self, stmt: &'b ast::Stmt) {
        let node_id = self.push_node(stmt);

        match stmt {
            Stmt::FunctionDef(ast::FunctionDefStmt {
                name,
                range,
                body,
                parameters,
                decorator_list,
                ..
            }) => {
                self.flags.insert(VisitorFlags::IN_FUNCTION);

                let (decl_id, _) = self.push_declaration(
                    name,
                    DeclarationKind::Stmt(DeclStmt::Function(ScopeId::sentinel())),
                    name.range,
                    node_id,
                );

                for decorator in decorator_list {
                    self.visit_decorator(decorator);
                }

                self.curr_scope = self.push_scope(ScopeKind::Function, *range);
                self.visit_parameters(parameters);

                // TODO: If the current file is from the python stdlib or "site-packages"
                // we should only visit the first statement of the function body.
                self.visit_body(body);

                let declaration = self.table.decls.get_mut(decl_id).unwrap();
                declaration.kind = if self.flags.contains(VisitorFlags::IN_CLASS) {
                    DeclarationKind::Stmt(DeclStmt::Method(
                        self.last_visited_class_symbol.unwrap(),
                        self.curr_scope,
                    ))
                } else {
                    DeclarationKind::Stmt(DeclStmt::Function(self.curr_scope))
                };

                self.flags.remove(VisitorFlags::IN_FUNCTION);
                self.pop_scope();
            }
            Stmt::ClassDef(ast::ClassDefStmt {
                name,
                range,
                body,
                decorator_list,
                arguments,
                ..
            }) => {
                let (decl_id, symbol_id) = self.push_declaration(
                    name,
                    DeclarationKind::Stmt(DeclStmt::Class(ScopeId::sentinel())),
                    name.range,
                    node_id,
                );
                self.last_visited_class_symbol = Some(symbol_id);

                for decorator in decorator_list {
                    self.visit_decorator(decorator);
                }
                if let Some(arguments) = arguments {
                    self.visit_arguments(arguments);
                }
                self.flags.insert(VisitorFlags::IN_CLASS);
                self.curr_scope = self.push_scope(ScopeKind::Class, *range);
                self.visit_body(body);

                let declaration = self.table.decls.get_mut(decl_id).unwrap();
                declaration.kind = DeclarationKind::Stmt(DeclStmt::Class(self.curr_scope));

                self.flags.remove(VisitorFlags::IN_CLASS);
                self.pop_scope();
            }
            Stmt::ImportFrom(import_from) => {
                let descriptor =
                    create_import_module_descriptor(AnyNodeRef::StmtImportFrom(import_from));
                let import_result = resolve_import(
                    self.file_info.path,
                    self.import_resolver_cfg.exec_env,
                    &descriptor,
                    self.import_resolver_cfg.config,
                    self.import_resolver_cfg.host,
                    self.import_resolver_cache.clone(),
                );
                let is_thirdparty = matches!(
                    import_result.import_type,
                    ImportType::ThirdParty | ImportType::BuiltIn
                );

                if let Some(module) = &import_from.module {
                    let mut pos = 0;
                    for (i, segment) in module
                        .split('.')
                        .filter(|part| !part.is_empty())
                        .enumerate()
                    {
                        let (stub_resolved_path, non_stub_resolved_path) =
                            if import_result.is_import_found {
                                let path = import_result.resolved_paths.get(i);

                                // If the `path` is a empty path, we have a namespace package. So, we're going
                                // to use the `package_directory` path instead.
                                let resolved_path =
                                    if path.is_some_and(|p| p.to_string_lossy().is_empty()) {
                                        import_result.package_directory.clone()
                                    } else {
                                        path.cloned()
                                    };

                                if import_result.is_stub_file {
                                    let non_stub_resolved_path = import_result
                                        .non_stub_import_result
                                        .as_ref()
                                        .and_then(|non_stub_import_result| {
                                            non_stub_import_result.resolved_paths.get(i)
                                        });
                                    (
                                        resolved_path.map(Arc::new),
                                        non_stub_resolved_path.cloned().map(Arc::new),
                                    )
                                } else {
                                    (None, resolved_path.map(Arc::new))
                                }
                            } else {
                                (None, None)
                            };

                        let start = module.range().start().to_u32() + pos;
                        let end = start + segment.len() as u32;
                        let segment_range = TextRange::new(start.into(), end.into());

                        self.push_declaration(
                            segment,
                            DeclarationKind::Stmt(DeclStmt::ImportSegment(ImportSource {
                                stub: stub_resolved_path,
                                non_stub: non_stub_resolved_path,
                                is_thirdparty,
                            })),
                            segment_range,
                            node_id,
                        );
                        pos += segment.len() as u32 + 1; // + 1 for the `.`
                    }
                }

                for name in &import_from.names {
                    self.push_node(name);
                    let (stub_resolved_path, non_stub_resolved_path) =
                        if let Some(implicit_import) =
                            // First, check if the name exists directly in implicit imports. If not found,
                            // check if it's a package by looking for an "__init__.py" file.
                            //
                            // Examples:
                            // 1. Direct module: "from foo import bar" -> looks for "bar"
                            // 2. Package import: "from foo import utils" -> if "utils" not found,
                            //    looks for "utils/__init__.py" as "utils" might be a package
                            import_result.implicit_imports.get(&name.name).or_else(|| {
                                    import_result
                                        .implicit_imports
                                        .get(&format!("{}/__init__.py", &name.name))
                                })
                        {
                            if implicit_import.is_stub_file {
                                (Some(Arc::new(implicit_import.path.clone())), None)
                            } else {
                                (None, Some(Arc::new(implicit_import.path.clone())))
                            }
                        } else {
                            // If the `path` is empty, we have a namespace package. If the import name
                            // isn't found in the implicit imports we return `None`.
                            if import_result
                                .resolved_paths
                                .last()
                                .is_some_and(|p| p.to_string_lossy().is_empty())
                            {
                                (None, None)
                            } else if import_result.is_stub_file {
                                let non_stub_resolved_path = import_result
                                    .non_stub_import_result
                                    .as_ref()
                                    .and_then(|non_stub_import_result| {
                                        non_stub_import_result.resolved_paths.last().cloned()
                                    });
                                (
                                    import_result.resolved_paths.last().cloned().map(Arc::new),
                                    non_stub_resolved_path.map(Arc::new),
                                )
                            } else {
                                (
                                    None,
                                    import_result.resolved_paths.last().cloned().map(Arc::new),
                                )
                            }
                        };
                    let import_src = ImportSource {
                        stub: stub_resolved_path,
                        non_stub: non_stub_resolved_path,
                        is_thirdparty,
                    };

                    let declaration_kind = if name.name.as_str() == "*" {
                        DeclarationKind::Stmt(DeclStmt::ImportStar(import_src))
                    } else {
                        DeclarationKind::Stmt(DeclStmt::Import(import_src))
                    };
                    let (decl_id, _) = self.push_declaration(
                        &name.name,
                        declaration_kind,
                        name.range,
                        self.curr_node.unwrap(),
                    );

                    if let Some(name) = name.asname.as_ref() {
                        self.push_declaration(
                            name,
                            DeclarationKind::Stmt(DeclStmt::ImportAlias(decl_id)),
                            name.range,
                            self.curr_node.unwrap(),
                        );
                    };

                    self.pop_node();
                }
            }
            Stmt::Import(ast::ImportStmt { names, .. }) => {
                for name in names {
                    self.push_node(name);
                    let descriptor = create_import_module_descriptor(AnyNodeRef::Alias(name));
                    let import_result = resolve_import(
                        self.file_info.path,
                        self.import_resolver_cfg.exec_env,
                        &descriptor,
                        self.import_resolver_cfg.config,
                        self.import_resolver_cfg.host,
                        self.import_resolver_cache.clone(),
                    );
                    let is_thirdparty = matches!(
                        import_result.import_type,
                        ImportType::ThirdParty | ImportType::BuiltIn
                    );

                    let mut pos = 0;
                    for (i, segment) in name
                        .name
                        .split('.')
                        .filter(|part| !part.is_empty())
                        .enumerate()
                    {
                        let is_first_segment = i == 0;

                        let (stub_resolved_path, non_stub_resolved_path) =
                            if import_result.is_import_found {
                                let path = import_result.resolved_paths.get(i);

                                // If the `path` is a empty path, we have a namespace package. So, we're going
                                // to use the `package_directory` path instead.
                                let resolved_path =
                                    if path.is_some_and(|p| p.to_string_lossy().is_empty()) {
                                        import_result.package_directory.clone()
                                    } else {
                                        path.cloned()
                                    };

                                if import_result.is_stub_file {
                                    let non_stub_resolved_path = import_result
                                        .non_stub_import_result
                                        .as_ref()
                                        .and_then(|non_stub_import_result| {
                                            non_stub_import_result.resolved_paths.get(i)
                                        });
                                    (
                                        resolved_path.map(Arc::new),
                                        non_stub_resolved_path.cloned().map(Arc::new),
                                    )
                                } else {
                                    (None, resolved_path.map(Arc::new))
                                }
                            } else {
                                (None, None)
                            };
                        let import_src = ImportSource {
                            stub: stub_resolved_path,
                            non_stub: non_stub_resolved_path,
                            is_thirdparty,
                        };

                        let start = name.range().start().to_u32() + pos;
                        let end = start + segment.len() as u32;
                        let segment_range = TextRange::new(start.into(), end.into());

                        let (decl_id, _) = self.push_declaration(
                            segment,
                            if is_first_segment {
                                DeclarationKind::Stmt(DeclStmt::Import(import_src))
                            } else {
                                DeclarationKind::Stmt(DeclStmt::ImportSegment(import_src))
                            },
                            segment_range,
                            self.curr_node.unwrap(),
                        );
                        pos += segment.len() as u32 + 1; // + 1 for the `.`

                        if let Some(name) = name.asname.as_ref() {
                            self.push_declaration(
                                name,
                                DeclarationKind::Stmt(DeclStmt::ImportAlias(decl_id)),
                                name.range,
                                self.curr_node.unwrap(),
                            );
                        };
                    }

                    self.pop_node();
                }
            }
            Stmt::TypeAlias(ast::TypeAliasStmt {
                name,
                type_params,
                value,
                ..
            }) => {
                self.set_declaration_node(node_id);
                self.visit_expr(name);

                if let Some(type_params) = type_params {
                    self.visit_type_params(type_params);
                }
                self.visit_expr(value);
                self.clear_declaration_node();
            }
            Stmt::Assign(ast::AssignStmt { targets, value, .. }) => {
                self.set_declaration_node(node_id);

                for target in targets {
                    self.visit_expr(target);
                }
                self.visit_expr(value);
                self.clear_declaration_node();
            }
            Stmt::AnnAssign(ast::AnnAssignStmt {
                target,
                annotation,
                value,
                ..
            }) => {
                self.set_declaration_node(node_id);

                self.visit_expr(target);
                self.visit_expr(annotation);

                if let Some(value) = value {
                    self.visit_expr(value);
                }

                self.clear_declaration_node();
            }
            Stmt::AugAssign(ast::AugAssignStmt {
                target, op, value, ..
            }) => {
                self.set_declaration_node(node_id);

                self.visit_expr(target);
                self.visit_operator(op);
                self.visit_expr(value);

                self.clear_declaration_node();
            }
            Stmt::For(ast::ForStmt {
                target,
                iter,
                body,
                orelse,
                ..
            }) => {
                self.visit_expr(iter);

                self.set_declaration_node(node_id);
                self.visit_expr(target);
                self.clear_declaration_node();

                self.visit_body(body);
                self.visit_body(orelse);
            }
            _ => visitor::walk_stmt(self, stmt),
        }

        self.pop_node();
    }

    fn visit_type_param(&mut self, type_param: &'b TypeParam) {
        let node_id = self.push_node(type_param);
        match type_param {
            TypeParam::TypeVar(type_var) => {
                self.push_declaration(
                    &type_var.name,
                    DeclarationKind::TypeVar,
                    type_var.range,
                    node_id,
                );
                if let Some(bound) = type_var.bound.as_ref() {
                    self.visit_expr(bound);
                }
                if let Some(default) = type_var.default.as_ref() {
                    self.visit_expr(default);
                }
            }
            TypeParam::ParamSpec(param_spec) => {
                self.push_declaration(
                    &param_spec.name,
                    DeclarationKind::TypeParamSpec,
                    param_spec.range,
                    node_id,
                );
                if let Some(default) = param_spec.default.as_ref() {
                    self.visit_expr(default);
                }
            }
            TypeParam::TypeVarTuple(type_var_tuple) => {
                self.push_declaration(
                    &type_var_tuple.name,
                    DeclarationKind::TypeVarTuple,
                    type_var_tuple.range,
                    node_id,
                );
                if let Some(default) = type_var_tuple.default.as_ref() {
                    self.visit_expr(default);
                }
            }
        }
        self.pop_node();
    }

    fn visit_except_handler(&mut self, except_handler: &'b ast::ExceptHandler) {
        self.push_node(except_handler);

        match except_handler {
            ast::ExceptHandler::ExceptHandler(ast::ExceptHandlerExceptHandler {
                name,
                body,
                ..
            }) => {
                if let Some(name) = name {
                    self.push_declaration(
                        name,
                        DeclarationKind::Exception,
                        name.range,
                        self.curr_node.unwrap(),
                    );
                }
                self.visit_body(body);
            }
        }
        self.pop_node();
    }

    fn visit_with_item(&mut self, with_item: &'b ast::WithItem) {
        let node_id = self.push_node(with_item);
        self.set_declaration_node(node_id);

        if let Some(optional_vars) = with_item.optional_vars.as_ref() {
            self.visit_expr(optional_vars);
        }
        self.visit_expr(&with_item.context_expr);

        self.pop_node();
        self.clear_declaration_node();
    }

    fn visit_parameters(&mut self, parameters: &'b ast::Parameters) {
        self.push_node(parameters);

        for (i, parameter_with_default) in parameters.iter_non_variadic_params().enumerate() {
            if i == 0 {
                self.flags.insert(VisitorFlags::IN_FIRST_PARAM);
            }

            self.push_node(parameter_with_default);
            self.visit_parameter(&parameter_with_default.parameter);
            if let Some(default) = &parameter_with_default.default {
                self.visit_expr(default);
            }
            self.pop_node();

            if i == 0 {
                self.flags.remove(VisitorFlags::IN_FIRST_PARAM);
            }
        }

        for parameter in parameters.iter().filter_map(AnyParameterRef::as_variadic) {
            self.visit_parameter(parameter);
        }

        self.pop_node();
    }

    fn visit_parameter(&mut self, parameter: &'b ast::Parameter) {
        self.push_node(parameter);
        let kind = if self.flags.contains(
            VisitorFlags::IN_CLASS | VisitorFlags::IN_FUNCTION | VisitorFlags::IN_FIRST_PARAM,
        ) {
            DeclarationKind::InstanceParameter(self.last_visited_class_symbol.unwrap())
        } else {
            DeclarationKind::Parameter
        };

        self.push_declaration(
            &parameter.name,
            kind,
            parameter.range,
            self.curr_node.unwrap(),
        );

        if let Some(annotation) = &parameter.annotation {
            self.visit_expr(annotation);
        }
        self.pop_node();
    }

    fn visit_pattern(&mut self, pattern: &'b Pattern) {
        let node_id = self.push_node(pattern);
        self.set_declaration_node(node_id);
        match pattern {
            Pattern::MatchAs(ast::PatternMatchAs { name, pattern, .. }) => {
                if let Some(pattern) = pattern {
                    self.visit_pattern(pattern);
                }
                if let Some(name) = name {
                    self.push_declaration(name, DeclarationKind::PatternMatch, name.range, node_id);
                }
            }
            Pattern::MatchSequence(ast::PatternMatchSequence { patterns, .. }) => {
                for pattern in patterns {
                    self.visit_pattern(pattern);
                }
            }
            _ => visitor::walk_pattern(self, pattern),
        }
        self.clear_declaration_node();
        self.pop_node();
    }

    fn visit_comprehension(&mut self, comprehension: &'b ast::Comprehension) {
        let node_id = self.push_node(comprehension);

        self.set_declaration_node(node_id);
        self.visit_expr(&comprehension.target);
        self.clear_declaration_node();

        self.visit_expr(&comprehension.iter);

        for expr in &comprehension.ifs {
            self.visit_expr(expr);
        }

        self.pop_node();
    }

    fn visit_f_string_element(&mut self, f_string_element: &'b ast::FStringElement) {
        self.push_node(f_string_element);
        visitor::walk_f_string_element(self, f_string_element);
        self.pop_node();
    }

    fn visit_expr(&mut self, expr: &'b ast::Expr) {
        let node_id = self.push_node(expr);
        match expr {
            Expr::Name(ast::NameExpr { id, ctx, range, .. }) => {
                if self.flags.contains(
                    VisitorFlags::IN_ATTRIBUTE_ASSIGNMENT
                        | VisitorFlags::IN_CLASS
                        | VisitorFlags::IN_FUNCTION,
                ) {
                    if let Some((_, symbol)) = self.table.lookup_symbol(id, self.curr_scope) {
                        let declaration = self
                            .table
                            .declaration(symbol.declarations().last())
                            .unwrap();
                        if matches!(declaration.kind, DeclarationKind::InstanceParameter(_)) {
                            self.flags.insert(VisitorFlags::IN_INSTANCE_ATTR_ASSIGNMENT);
                        }
                    }
                }
                self.handle_context(ctx, id, range)
            }
            Expr::Attribute(ast::AttributeExpr {
                value,
                attr,
                ctx,
                range,
            }) => {
                if ctx.is_store() {
                    self.flags.insert(VisitorFlags::IN_ATTRIBUTE_ASSIGNMENT);
                }

                self.visit_expr(value);
                self.handle_context(ctx, attr, range);

                self.flags.remove(VisitorFlags::IN_ATTRIBUTE_ASSIGNMENT);
            }
            Expr::Lambda(ast::LambdaExpr {
                body,
                parameters,
                range,
            }) => {
                self.curr_scope = self.push_scope(ScopeKind::Lambda, *range);
                if let Some(parameters) = parameters {
                    self.visit_parameters(parameters);
                }
                self.visit_expr(body);
                self.pop_scope();
            }
            Expr::ListComp(ast::ListCompExpr {
                elt,
                generators,
                range,
            }) => {
                self.curr_scope = self.push_scope(ScopeKind::Comprehension, *range);
                for comprehension in generators {
                    self.visit_comprehension(comprehension);
                }
                self.visit_expr(elt);
                self.pop_scope();
            }
            Expr::SetComp(ast::SetCompExpr {
                elt,
                generators,
                range,
            }) => {
                self.curr_scope = self.push_scope(ScopeKind::Comprehension, *range);
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
                range,
            }) => {
                self.curr_scope = self.push_scope(ScopeKind::Comprehension, *range);
                for comprehension in generators {
                    self.visit_comprehension(comprehension);
                }
                self.visit_expr(key);
                self.visit_expr(value);
                self.pop_scope();
            }
            Expr::Generator(ast::GeneratorExpr {
                elt,
                generators,
                range,
                ..
            }) => {
                self.curr_scope = self.push_scope(ScopeKind::Comprehension, *range);
                for comprehension in generators {
                    self.visit_comprehension(comprehension);
                }
                self.visit_expr(elt);
                self.pop_scope();
            }
            Expr::Named(ast::NamedExpr { target, value, .. }) => {
                self.set_declaration_node(node_id);
                self.visit_expr(target);
                self.clear_declaration_node();

                self.visit_expr(value);
            }
            _ => visitor::walk_expr(self, expr),
        }
        self.pop_node();
    }
}
