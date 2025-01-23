use std::{
    fs::{self},
    hash::BuildHasherDefault,
    ops::Deref,
    path::{Path, PathBuf},
};

use bimap::BiHashMap;
use python_ast::ModModule;
use python_ast_utils::nodes::NodeStack;
use python_parser::{parse_module, Parsed};
use rayon::iter::{IntoParallelIterator, ParallelIterator};
use ruff_index::{newtype_index, Idx};
use ruff_python_resolver::{
    config::Config,
    execution_environment::ExecutionEnvironment,
    host::{Host, StaticHost},
    python_platform::PythonPlatform,
    python_version::PythonVersion,
};
use rustc_hash::{FxHashMap, FxHasher};
use walkdir::WalkDir;

use crate::{
    declaration::{DeclId, DeclStmt, Declaration, DeclarationKind, DeclarationQuery},
    symbol::{Symbol, SymbolId, SymbolOccurrence},
    symbol_table::{ImportResolverConfig, SymbolTable, SymbolTableBuilder},
    vendored::setup_typeshed,
    Scope, ScopeId, ScopeKind,
};

pub enum Source<'a> {
    New(&'a str),
    Update { old: &'a str, new: &'a str },
}

type FxBiHashMap<L, R> =
    BiHashMap<L, R, BuildHasherDefault<FxHasher>, BuildHasherDefault<FxHasher>>;

#[newtype_index]
pub struct FileId;

#[derive(Debug, Default, Clone)]
pub struct Files {
    paths: FxBiHashMap<PathBuf, FileId>,
}

impl Files {
    fn contains_path(&self, path: &PathBuf) -> bool {
        self.paths.contains_left(path)
    }

    fn insert(&mut self, path: PathBuf) -> FileId {
        let next_index = FileId::new(self.paths.len() + 1);
        self.paths
            .insert_no_overwrite(path, next_index)
            .expect("Tried to insert repeated value");
        next_index
    }

    fn get_by_id(&self, file_id: &FileId) -> Option<&PathBuf> {
        self.paths.get_by_right(file_id)
    }

    fn get_by_path(&self, path: &PathBuf) -> Option<&FileId> {
        self.paths.get_by_left(path)
    }
}

#[derive(Debug, Clone)]
pub struct BuiltinSymbolTable {
    table: SymbolTable,
    ast: Parsed<ModModule>,
    path: PathBuf,
}

impl BuiltinSymbolTable {
    pub fn path(&self) -> &PathBuf {
        &self.path
    }

    pub fn ast(&self) -> &Vec<python_ast::Stmt> {
        self.ast.suite()
    }

    pub fn symbol_table(&self) -> &SymbolTable {
        &self.table
    }

    pub fn scope_id(&self) -> ScopeId {
        ScopeId::global()
    }

    pub fn scope(&self) -> &Scope {
        self.table.scope(self.scope_id()).unwrap()
    }

    pub fn references(&self, name: &str) -> Option<&Vec<SymbolOccurrence>> {
        self.table.references(name, self.scope_id())
    }

    pub fn symbol_declaration(&self, name: &str) -> Option<&Declaration> {
        self.table
            .symbol_declaration(name, self.scope_id(), DeclarationQuery::Last)
    }

    pub fn node_stack(&self) -> NodeStack {
        NodeStack::default().build(self.ast())
    }
}

impl Deref for BuiltinSymbolTable {
    type Target = SymbolTable;

    fn deref(&self) -> &Self::Target {
        &self.table
    }
}

impl Default for BuiltinSymbolTable {
    fn default() -> Self {
        Self {
            table: SymbolTable::default(),
            ast: Parsed::default(),
            path: PathBuf::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Indexer {
    tables: FxHashMap<FileId, SymbolTable>,
    asts: FxHashMap<FileId, Parsed<ModModule>>,
    files: Files,

    exec_env: ExecutionEnvironment,
    config: Config,
    host: StaticHost,

    was_root_path_indexed: bool,
}

impl Indexer {
    fn new(
        root: PathBuf,
        python_version: PythonVersion,
        python_platform: PythonPlatform,
        python_search_paths: Vec<PathBuf>,
    ) -> Self {
        Self {
            tables: FxHashMap::default(),
            asts: FxHashMap::default(),
            files: Files::default(),
            exec_env: ExecutionEnvironment {
                root,
                python_version,
                python_platform,
                // TODO: add settings option in the LSP for this
                extra_paths: vec![],
            },
            config: Config {
                // TODO: add settings option in the LSP for this
                typeshed_path: Some(setup_typeshed()),
                stub_path: None,
                venv_path: None,
                venv: None,
            },
            host: StaticHost::new(python_search_paths),
            was_root_path_indexed: false,
        }
    }

    /// Collects Python files (*.py) from a given directory path based on project indicators.
    ///
    /// This function scans a directory for Python files with the following behavior:
    /// - If the directory contains any of these project indicators:
    ///   * pyproject.toml
    ///   * requirements.txt
    ///   * .git directory
    ///     then it will recursively scan all subdirectories for Python files.
    /// - If none of these indicators are present, it will only scan the immediate directory
    ///   (non-recursively) for Python files.
    fn scan_dir_for_python_files(path: impl AsRef<Path>) -> Vec<PathBuf> {
        let path = path.as_ref();
        assert!(path.is_dir());

        let mut python_files = Vec::new();

        let has_pyproject_file = path.join("pyproject.toml").exists();
        let has_requirements_file = path.join("requirements.txt").exists();
        let has_git_dir = path.join(".git").is_dir();

        let should_scan_subdirs = has_pyproject_file || has_requirements_file || has_git_dir;
        let max_scan_depth = if should_scan_subdirs { usize::MAX } else { 1 };

        for entry in WalkDir::new(path)
            .max_depth(max_scan_depth)
            .into_iter()
            .filter_entry(|e| !e.path().ends_with(".venv"))
            .filter_map(|e| e.ok())
        {
            let path = entry.path();
            if path.extension().is_some_and(|e| e == "py") {
                python_files.push(path.to_path_buf());
            }
        }

        python_files
    }

    fn index_content(
        &self,
        path: impl AsRef<Path>,
        content: &str,
    ) -> (SymbolTable, Parsed<ModModule>, Vec<PathBuf>) {
        let parsed_file = parse_module(content);
        let table = SymbolTableBuilder::new(
            path.as_ref(),
            ImportResolverConfig::new(&self.exec_env, &self.config, &self.host),
        )
        .build(parsed_file.suite());

        let mut import_paths = Vec::new();
        for import_decl in table
            .declarations()
            .filter(|decl| matches!(decl.kind, DeclarationKind::Stmt(DeclStmt::Import { .. })))
        {
            let DeclarationKind::Stmt(DeclStmt::Import { source }) = &import_decl.kind else {
                unreachable!()
            };
            if let Some(source) = source {
                import_paths.push(source.clone());
            }
        }

        (table, parsed_file, import_paths)
    }

    fn index_files(&mut self, mut file_ids: Vec<FileId>) {
        loop {
            let tables: Vec<_> = file_ids
                .into_par_iter()
                .filter_map(|file_id| {
                    let path = self.file_path(&file_id);
                    if path.is_file() && path.extension().is_some_and(|ext| ext != "so") {
                        let content = fs::read_to_string(path)
                            .unwrap_or_else(|e| panic!("{e}: {}", path.display()));
                        let (table, parsed_file, import_paths) = self.index_content(path, &content);

                        Some((file_id, table, parsed_file, import_paths))
                    } else {
                        None
                    }
                })
                .collect();

            // files that will be parsed later
            let mut deferred_file_ids = Vec::new();
            for (file_id, table, parsed_file, import_paths) in tables {
                // collect the resolved import paths to be parsed later
                deferred_file_ids.extend(self.push_file_ids(import_paths));

                self.tables.insert(file_id, table);
                self.asts.insert(file_id, parsed_file);
            }

            if deferred_file_ids.is_empty() {
                break;
            } else {
                file_ids = deferred_file_ids;
            }
        }
    }

    fn tables(&self) -> impl Iterator<Item = (&FileId, &SymbolTable)> {
        self.tables.iter()
    }

    fn push_file(&mut self, file_path: PathBuf) -> FileId {
        self.files.insert(file_path)
    }

    pub fn typeshed_path(&self) -> &PathBuf {
        self.config.typeshed_path.as_ref().unwrap()
    }

    fn contains_path(&self, path: &PathBuf) -> bool {
        self.files.contains_path(path)
    }

    pub fn root_path(&self) -> &Path {
        &self.exec_env.root
    }

    pub fn python_search_paths(&self) -> Vec<PathBuf> {
        self.host.python_search_paths()
    }

    /// Returns the path [`PathBuf`] for a given `file_id`.
    /// # Panics
    /// If there's no [`PathBuf`] for a `file_id`.
    pub fn file_path(&self, file_id: &FileId) -> &PathBuf {
        self.files
            .get_by_id(file_id)
            .unwrap_or_else(|| panic!("no `file_path` for {:?}", file_id))
    }

    /// Returns the [`FileId`] for a given `file_path`.
    /// # Panics
    /// If there's no [`FileId`] for a `file_path`.
    pub fn file_id(&self, file_path: &PathBuf) -> FileId {
        self.files
            .get_by_path(file_path)
            .copied()
            .unwrap_or_else(|| panic!("no `file_id` for {}", file_path.display()))
    }

    pub fn ast(&self, file: &PathBuf) -> Option<&Parsed<ModModule>> {
        self.asts.get(self.files.get_by_path(file)?)
    }

    fn push_file_ids(&mut self, paths: Vec<PathBuf>) -> Vec<FileId> {
        paths
            .into_iter()
            .filter_map(|path| {
                if self.contains_path(&path) {
                    None
                } else {
                    Some(self.push_file(path))
                }
            })
            .collect()
    }

    pub fn add_or_update_file(&mut self, file_path: PathBuf, source: Source) {
        let deferred_paths = match source {
            Source::New(source) => {
                if self.contains_path(&file_path) {
                    return;
                }

                let (table, parsed_file, mut import_paths) = self.index_content(&file_path, source);
                let file_id = self.push_file(file_path);

                self.tables.insert(file_id, table);
                self.asts.insert(file_id, parsed_file);

                if !self.was_root_path_indexed {
                    self.was_root_path_indexed = true;

                    let python_files = Indexer::scan_dir_for_python_files(&self.exec_env.root);
                    import_paths.extend(python_files);
                }

                import_paths
            }
            Source::Update { new, .. } => {
                let file_id = self.file_id(&file_path);
                let (table, parsed_file, import_paths) = self.index_content(file_path, new);

                self.tables.insert(file_id, table);
                self.asts.insert(file_id, parsed_file);

                import_paths
            }
        };

        let file_ids = self.push_file_ids(deferred_paths);
        self.index_files(file_ids);
    }
}

#[derive(Debug, Clone)]
pub struct SymbolTableDb {
    index: Indexer,
    builtin_symbol_table: BuiltinSymbolTable,
}

impl SymbolTableDb {
    pub fn new(
        root: PathBuf,
        python_version: PythonVersion,
        python_platform: PythonPlatform,
        python_search_paths: Vec<PathBuf>,
    ) -> Self {
        Self {
            index: Indexer::new(root, python_version, python_platform, python_search_paths),
            builtin_symbol_table: BuiltinSymbolTable::default(),
        }
    }

    pub fn indexer(&self) -> &Indexer {
        &self.index
    }

    pub fn indexer_mut(&mut self) -> &mut Indexer {
        &mut self.index
    }

    pub fn with_builtin_symbols(mut self) -> Self {
        let builtins_path = self.indexer().typeshed_path().join("stdlib/builtins.pyi");
        let content = fs::read_to_string(&builtins_path).expect("builtins.pyi file to exist");

        let (table, parsed_file, resolved_paths) =
            self.indexer_mut().index_content(&builtins_path, &content);

        self.builtin_symbol_table = BuiltinSymbolTable {
            table,
            ast: parsed_file,
            path: builtins_path,
        };

        let file_ids = self.indexer_mut().push_file_ids(resolved_paths);
        self.indexer_mut().index_files(file_ids);

        self
    }

    pub fn builtin_symbols(&self) -> &BuiltinSymbolTable {
        &self.builtin_symbol_table
    }

    pub fn table(&self, file: &PathBuf) -> &SymbolTable {
        match self.indexer().tables.get(&self.indexer().file_id(file)) {
            Some(table) => table,
            None => {
                if file.ends_with("stdlib/builtins.pyi") {
                    &self.builtin_symbol_table.table
                } else {
                    panic!("no symbol table for path: {}", file.display());
                }
            }
        }
    }

    // TODO: maybe this should return an Option
    pub fn symbol(&self, file: &PathBuf, symbol_id: SymbolId) -> &Symbol {
        self.table(file)
            .symbol(symbol_id)
            .or_else(|| self.builtin_symbols().symbol(symbol_id))
            .expect("symbol for id")
    }

    pub fn lookup_symbol(&self, file: &PathBuf, name: &str, scope: ScopeId) -> Option<&Symbol> {
        self.table(file).lookup_symbol(name, scope).or_else(|| {
            self.builtin_symbols()
                .lookup_symbol(name, ScopeId::global())
        })
    }

    pub fn symbol_name(&self, file: &PathBuf, symbol_id: SymbolId) -> &str {
        let symbol = self.symbol(file, symbol_id);
        let scope = self.scope(file, symbol.definition_scope());
        scope.symbol_name(symbol_id).unwrap()
    }

    pub fn declaration(&self, file: &PathBuf, decl_id: DeclId) -> &Declaration {
        self.table(file)
            .declaration(decl_id)
            .expect("expect declaration for id")
    }

    pub fn symbol_declaration(
        &self,
        file: &PathBuf,
        name: &str,
        scope_id: ScopeId,
        query: DeclarationQuery,
    ) -> Option<&Declaration> {
        self.table(file)
            .symbol_declaration(name, scope_id, query)
            .or_else(|| self.builtin_symbols().symbol_declaration(name))
    }

    pub fn find_enclosing_scope(&self, file: &PathBuf, offset: u32) -> (ScopeId, &Scope) {
        self.table(file).find_enclosing_scope(offset)
    }

    pub fn references(
        &self,
        file: &PathBuf,
        name: &str,
        offset: u32,
    ) -> Option<FxHashMap<FileId, Vec<SymbolOccurrence>>> {
        let global_scope = ScopeId::global();
        let mut result: FxHashMap<FileId, Vec<SymbolOccurrence>> = FxHashMap::default();
        // add the references from the current `file`
        let references = self
            .table(file)
            .references(name, global_scope)
            .or(self.builtin_symbols().references(name))?;
        let current_file_id = self.indexer().file_id(file);
        result.insert(current_file_id, references.clone());

        let declaration =
            self.symbol_declaration(file, name, global_scope, DeclarationQuery::AtOffset(offset))?;

        let symbol = self.symbol(file, declaration.symbol_id);
        let scope = self.scope(file, symbol.definition_scope());

        // short circuit if the symbol was declared locally to a scope
        if matches!(
            scope.kind(),
            ScopeKind::Function | ScopeKind::Class | ScopeKind::Lambda | ScopeKind::Comprehension
        ) {
            return Some(result);
        }

        let origin_path = if let DeclarationKind::Stmt(DeclStmt::Import {
            source: Some(source),
        }) = &declaration.kind
        {
            // add the references from the source file of the imported symbol
            let references = self
                .table(source)
                .references(name, global_scope)
                .or(self.builtin_symbols().references(name))?;
            let source_id = self.indexer().file_id(source);
            result.insert(source_id, references.clone());

            source
        } else {
            // If the symbol was not imported then it was declared in `file`, therefore we need to
            // look for symbols that were imported and contains the same path as `file`.
            file
        };

        // search for imported symbols that have the same path as `origin_path` and add the
        // references from their files
        for (file_id, table) in self
            .indexer()
            .tables()
            // filter the already processed file
            .filter(|(&file_id, _)| file_id != current_file_id)
        {
            if let Some(DeclarationKind::Stmt(DeclStmt::Import { source })) = table
                .symbol_declaration(name, global_scope, DeclarationQuery::Last)
                .map(|decl| &decl.kind)
            {
                if source.as_ref().is_some_and(|path| path == origin_path) {
                    let Some(references) = table.references(name, global_scope) else {
                        continue;
                    };
                    result.insert(*file_id, references.clone());
                }
            }
        }

        Some(result)
    }

    /// Returns the global [`Scope`] of a `file`.
    pub fn global_scope(&self, file: &PathBuf) -> &Scope {
        self.table(file).scope(ScopeId::global()).unwrap()
    }

    pub fn scope(&self, file: &PathBuf, scope_id: ScopeId) -> &Scope {
        self.table(file).scope(scope_id).expect("scope to exist")
    }

    pub fn scopes(&self, file: &PathBuf) -> impl Iterator<Item = &Scope> {
        self.table(file).scopes.iter()
    }
}

#[cfg(test)]
mod tests {
    use python_utils::{get_python_platform, get_python_search_paths, get_python_version};
    use ruff_text_size::TextRange;
    use std::path::{Path, PathBuf};

    use crate::symbol::SymbolOccurrence;

    use super::{Source, SymbolTableDb};

    fn resolve_relative_path(path: impl AsRef<Path>) -> PathBuf {
        let curr_dir = std::env::current_dir().expect("Failed to get current dir");
        if path.as_ref().starts_with("..") || path.as_ref().starts_with(".") {
            let path: PathBuf = path
                .as_ref()
                .components()
                .filter(|c| {
                    !matches!(
                        c,
                        std::path::Component::ParentDir | std::path::Component::CurDir
                    )
                })
                .collect();
            curr_dir.join(path)
        } else {
            curr_dir.join(path)
        }
    }

    fn setup_db(path: impl AsRef<Path>, root: impl AsRef<Path>) -> crate::db::SymbolTableDb {
        let src = std::fs::read_to_string(&path).unwrap();
        let search_paths = get_python_search_paths("/usr/bin/python");
        let python_version = get_python_version("/usr/bin/python").unwrap();
        let python_platform = get_python_platform().unwrap();
        let mut db = SymbolTableDb::new(
            root.as_ref().to_path_buf(),
            python_version,
            python_platform,
            search_paths.paths,
        )
        .with_builtin_symbols();
        db.indexer_mut()
            .add_or_update_file(path.as_ref().to_path_buf(), Source::New(&src));
        db
    }

    #[test]
    fn test_references() {
        let root = resolve_relative_path("../resources/tests/fixtures/small_project/");
        let path = resolve_relative_path(
            "../resources/tests/fixtures/small_project/tim/api/routes/users.py",
        );
        let db = setup_db(&path, &root);
        let mut references = db
            .references(&path, "get_current_user", 204)
            .map(|refs| {
                refs.into_iter()
                    .map(|(file_id, ranges)| (db.indexer().file_path(&file_id), ranges))
                    .collect::<Vec<_>>()
            })
            .unwrap();
        references.sort_by(|(a, _), (b, _)| a.cmp(b));

        assert_eq!(
            references,
            vec![
                (
                    &resolve_relative_path(
                        "../resources/tests/fixtures/small_project/tim/api/dependencies.py"
                    ),
                    vec![
                        SymbolOccurrence::Declaration(TextRange::new(506.into(), 522.into())),
                        SymbolOccurrence::Reference(TextRange::new(1350.into(), 1366.into()))
                    ]
                ),
                (
                    &resolve_relative_path(
                        "../resources/tests/fixtures/small_project/tim/api/routes/items.py"
                    ),
                    vec![
                        SymbolOccurrence::Declaration(TextRange::new(162.into(), 178.into())),
                        SymbolOccurrence::Reference(TextRange::new(285.into(), 301.into())),
                        SymbolOccurrence::Reference(TextRange::new(522.into(), 538.into())),
                        SymbolOccurrence::Reference(TextRange::new(747.into(), 763.into())),
                        SymbolOccurrence::Reference(TextRange::new(1149.into(), 1165.into())),
                        SymbolOccurrence::Reference(TextRange::new(1533.into(), 1549.into())),
                        SymbolOccurrence::Reference(TextRange::new(1961.into(), 1977.into())),
                    ]
                ),
                (
                    &resolve_relative_path(
                        "../resources/tests/fixtures/small_project/tim/api/routes/users.py"
                    ),
                    vec![
                        SymbolOccurrence::Declaration(TextRange::new(180.into(), 196.into())),
                        SymbolOccurrence::Reference(TextRange::new(953.into(), 969.into())),
                        SymbolOccurrence::Reference(TextRange::new(2301.into(), 2317.into())),
                        SymbolOccurrence::Reference(TextRange::new(2937.into(), 2953.into())),
                        SymbolOccurrence::Reference(TextRange::new(2967.into(), 2983.into())),
                    ]
                ),
            ]
        );
    }
}
