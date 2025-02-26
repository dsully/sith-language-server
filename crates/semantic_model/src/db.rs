use std::{
    fs::{self},
    hash::BuildHasherDefault,
    ops::Deref,
    path::{Path, PathBuf},
};

use bimap::BiHashMap;
use python_ast::{ModModule, Suite};
use python_ast_utils::nodes::NodeStack;
use python_parser::{parse_module, Parsed};
use python_utils::{PythonHost, ROOT_FILES};
use rayon::iter::{IntoParallelIterator, ParallelIterator};
use ruff_index::{newtype_index, Idx};
use ruff_python_resolver::{
    config::Config, execution_environment::ExecutionEnvironment, host::Host,
};
use rustc_hash::{FxHashMap, FxHasher};
use serde::{Deserialize, Serialize};
use walkdir::WalkDir;

use crate::{
    declaration::{DeclId, Declaration, DeclarationQuery},
    symbol::{Symbol, SymbolId},
    symbol_table::{ImportResolverConfig, SymbolTable, SymbolTableBuilder},
    vendored::setup_typeshed,
    Scope, ScopeId,
};

pub enum Source<'a> {
    New(&'a str),
    Update { old: &'a str, new: &'a str },
}

type FxBiHashMap<L, R> =
    BiHashMap<L, R, BuildHasherDefault<FxHasher>, BuildHasherDefault<FxHasher>>;

#[newtype_index]
#[derive(Serialize, Deserialize)]
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

    pub fn suite(&self) -> &Suite {
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

    pub fn lookup_symbol(&self, name: &str, scope_id: ScopeId) -> Option<&Symbol> {
        self.table.lookup_symbol(name, scope_id).filter(|symbol| {
            self.table
                .declaration(symbol.declarations().last())
                .is_some_and(|declaration| !declaration.is_import())
        })
    }

    pub fn symbol_declaration(&self, name: &str) -> Option<&Declaration> {
        self.table
            .symbol_declaration(name, self.scope_id(), DeclarationQuery::Last)
            .filter(|&declaration| !declaration.is_import())
    }

    pub fn node_stack(&self) -> NodeStack {
        NodeStack::default().build(self.suite())
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
    builtin_symbol_table: BuiltinSymbolTable,

    exec_env: ExecutionEnvironment,
    config: Config,
    host: PythonHost,

    was_root_path_indexed: bool,
    collection_stub_path: PathBuf,
}

impl Indexer {
    fn new(root: PathBuf, python_host: PythonHost) -> Self {
        let typeshed_path = setup_typeshed();
        Self {
            tables: FxHashMap::default(),
            asts: FxHashMap::default(),
            files: Files::default(),
            builtin_symbol_table: BuiltinSymbolTable::default(),
            collection_stub_path: typeshed_path.join("stdlib/collections/__init__.pyi"),
            exec_env: ExecutionEnvironment {
                root,
                python_version: python_host.version,
                python_platform: python_host.platform,
                // TODO: add settings option in the LSP for this
                extra_paths: vec![],
            },
            config: Config {
                // TODO: add settings option in the LSP for this
                typeshed_path: Some(typeshed_path),
                stub_path: None,
                venv_path: None,
                venv: None,
            },
            host: python_host,
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
    ///   * Pipfile
    ///   * setup.py
    ///     then it will recursively scan all subdirectories for Python files.
    /// - If none of these indicators are present, it will only scan the immediate directory
    ///   (non-recursively) for Python files.
    fn scan_dir_for_python_files(root_dir: impl AsRef<Path>) -> Vec<PathBuf> {
        let root_dir = root_dir.as_ref();
        assert!(root_dir.is_dir());

        let mut python_files = Vec::new();

        let should_scan_subdirs = fs::read_dir(root_dir)
            .unwrap()
            .filter_map(|e| e.ok())
            .any(|e| {
                let path = e.path();
                let file_name = path.file_name().map(|f| f.to_string_lossy()).unwrap();
                ROOT_FILES.contains(file_name.as_ref())
            });
        let max_scan_depth = if should_scan_subdirs { usize::MAX } else { 1 };

        for entry in WalkDir::new(root_dir)
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
        file_id: FileId,
        content: &str,
    ) -> (SymbolTable, Parsed<ModModule>, Vec<PathBuf>) {
        let path = self.file_path(&file_id);
        let parsed_file = parse_module(content);
        let table = SymbolTableBuilder::new(
            path,
            file_id,
            ImportResolverConfig::new(&self.exec_env, &self.config, &self.host),
        )
        .build(parsed_file.suite());

        let mut import_paths = Vec::new();
        for import_source in table
            .declarations()
            .filter_map(|declaration| declaration.import_source())
        {
            import_paths.push(import_source.clone());
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
                        let (table, parsed_file, import_paths) =
                            self.index_content(file_id, &content);

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

    fn index_builtin_symbols(&mut self) {
        let builtins_path = self.typeshed_path().join("stdlib/builtins.pyi");
        let content = fs::read_to_string(&builtins_path).expect("builtins.pyi file not found");

        let file_id = self.push_file(builtins_path.clone());
        let (table, parsed_file, _) = self.index_content(file_id, &content);

        self.builtin_symbol_table = BuiltinSymbolTable {
            table,
            ast: parsed_file,
            path: builtins_path,
        };
    }

    fn index_collection_types(&mut self) {
        let content = fs::read_to_string(&self.collection_stub_path)
            .expect("collections/__init__.pyi file not found");

        let file_id = self.push_file(self.collection_stub_path.clone());
        let (table, parsed_file, _) = self.index_content(file_id, &content);
        self.tables.insert(file_id, table);
        self.asts.insert(file_id, parsed_file);
    }

    pub fn tables(&self) -> impl Iterator<Item = (&FileId, &SymbolTable)> {
        self.tables.iter()
    }

    fn push_file(&mut self, file_path: PathBuf) -> FileId {
        self.files.insert(file_path)
    }

    pub fn typeshed_path(&self) -> &PathBuf {
        self.config.typeshed_path.as_ref().unwrap()
    }

    pub fn contains_path(&self, path: &PathBuf) -> bool {
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

    pub fn ast(&self, file: &PathBuf) -> &Parsed<ModModule> {
        self.files
            .get_by_path(file)
            .and_then(|file_id| self.asts.get(file_id))
            .expect("AST for provided file")
    }

    pub fn node_stack(&self, file: &PathBuf) -> NodeStack {
        let suite = self.ast(file).suite();
        NodeStack::default().build(suite)
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

                let file_id = self.push_file(file_path);
                let (table, parsed_file, mut import_paths) = self.index_content(file_id, source);

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
                let (table, parsed_file, import_paths) = self.index_content(file_id, new);

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
}

impl SymbolTableDb {
    pub fn new(root: PathBuf, python_host: PythonHost) -> Self {
        Self {
            index: Indexer::new(root, python_host),
        }
    }

    pub fn indexer(&self) -> &Indexer {
        &self.index
    }

    pub fn indexer_mut(&mut self) -> &mut Indexer {
        &mut self.index
    }

    pub fn with_builtin_symbols(mut self) -> Self {
        self.indexer_mut().index_builtin_symbols();
        self
    }

    pub fn with_collection_types(mut self) -> Self {
        self.indexer_mut().index_collection_types();
        self
    }

    pub fn builtin_symbols(&self) -> &BuiltinSymbolTable {
        &self.indexer().builtin_symbol_table
    }

    pub fn collection_stub_symbol_table(&self) -> &SymbolTable {
        self.table(self.collection_stub_path())
    }

    pub fn collection_stub_path(&self) -> &PathBuf {
        &self.index.collection_stub_path
    }

    pub fn table(&self, file: &PathBuf) -> &SymbolTable {
        match self.indexer().tables.get(&self.indexer().file_id(file)) {
            Some(table) => table,
            None if file.ends_with("stdlib/builtins.pyi") => &self.builtin_symbols().table,
            _ => panic!("no symbol table for path: {}", file.display()),
        }
    }

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
