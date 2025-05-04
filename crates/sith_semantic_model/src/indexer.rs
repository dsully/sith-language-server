use std::{
    hash::BuildHasherDefault,
    ops::Deref,
    path::{Path, PathBuf},
    sync::{Arc, Mutex, RwLock},
    time::Instant,
};

use bimap::BiHashMap;
use ruff_index::newtype_index;
use ruff_python_resolver::{
    cache::ImportResolverCache, config::Config, execution_environment::ExecutionEnvironment,
    host::Host,
};
use rustc_hash::{FxBuildHasher, FxHashMap, FxHashSet, FxHasher};
use serde::{Deserialize, Serialize};
use sith_python_ast::{ModModule, Suite};
use sith_python_ast_utils::nodes::NodeStack;
use sith_python_parser::{parse_module, Parsed};
use sith_python_utils::{PythonHost, ROOT_FILES};
use walkdir::WalkDir;

use crate::{
    db::Source,
    declaration::{Declaration, DeclarationQuery, ImportSource},
    symbol_table::{ImportResolverConfig, SymbolTable, SymbolTableBuilder},
    util, Scope, ScopeId, Symbol, SymbolId,
};

type FxBiHashMap<L, R> =
    BiHashMap<L, R, BuildHasherDefault<FxHasher>, BuildHasherDefault<FxHasher>>;

#[newtype_index]
#[derive(Serialize, Deserialize)]
pub struct FileId;

impl Default for FileId {
    fn default() -> Self {
        FileId::from_usize(0)
    }
}

#[derive(Debug, Default, Clone)]
pub struct Files {
    stub_paths: FxBiHashMap<Arc<PathBuf>, FileId>,
    non_stub_paths: FxBiHashMap<Arc<PathBuf>, FileId>,
    indexed: FxHashSet<Arc<PathBuf>>,
    next_id_value: FileId,
}

impl Files {
    fn contains_file(&self, path: &PathBuf) -> bool {
        self.stub_paths.contains_left(path) || self.non_stub_paths.contains_left(path)
    }

    pub fn get_or_assign_id(&mut self, path: Arc<PathBuf>) -> FileId {
        if let Some(id) = self.any_file_id(&path) {
            return id;
        }

        let file_id = self.next_id_value;
        self.next_id_value = self.next_id_value + 1;

        if path.extension().is_some_and(|ext| ext == "pyi") {
            self.stub_paths
                .insert_no_overwrite(path, file_id)
                .expect("Tried to insert repeated value");
        } else {
            self.non_stub_paths
                .insert_no_overwrite(path, file_id)
                .expect("Tried to insert repeated value");
        }
        file_id
    }

    /// Add the [`ImportSource`] to the indexer, creating the corresponding [`FileId`]
    /// for the stub or non-stub path. If [`ImportSource`] has both stub and non-stub path, the
    /// [`FileId`] created will be associated for both of them.
    ///
    /// Returns a list of `(FileId, bool)`, where:
    /// - `FileId` is the unique id for the registered path
    /// - `bool` indicates whether the path is third-party (from the stdlib or site-packages)
    fn push_source(&mut self, source: ImportSource) {
        // Skip if both paths are None
        if source.stub.is_none() && source.non_stub.is_none() {
            return;
        }

        // Case 1: We have a stub path (.pyi file)
        if let Some(stub_path) = source.stub {
            // Skip if already indexed
            if self.contains_file(&stub_path) {
                return;
            }

            // Register the stub path
            let file_id = self.get_or_assign_id(stub_path.clone());
            // self.indexed.insert(stub_path);

            // If we also have a non-stub path, associate it with the same file ID
            if let Some(non_stub_path) = source.non_stub {
                // Associate the non-stub path with the same file ID if not already indexed
                if !self.contains_file(&non_stub_path) {
                    self.non_stub_paths
                        .insert_no_overwrite(non_stub_path, file_id)
                        .expect("Tried to insert duplicated value");
                }
            }

            return;
        }

        // Case 2: We only have a non-stub path (.py file)
        if let Some(non_stub_path) = source.non_stub {
            // Skip if already indexed
            if self.contains_file(&non_stub_path) {
                return;
            }

            // Register the non-stub path
            self.get_or_assign_id(non_stub_path.clone());
            // self.indexed.insert(non_stub_path);
        }
    }

    fn any_path(&self, file_id: &FileId) -> Option<&Arc<PathBuf>> {
        self.stub_path(file_id).or(self.non_stub_path(file_id))
    }

    fn any_file_id(&self, path: &PathBuf) -> Option<FileId> {
        self.stub_file_id(path)
            .or(self.non_stub_file_id(path))
            .copied()
    }

    fn stub_path(&self, file_id: &FileId) -> Option<&Arc<PathBuf>> {
        self.stub_paths.get_by_right(file_id)
    }

    fn stub_file_id(&self, path: &PathBuf) -> Option<&FileId> {
        self.stub_paths.get_by_left(path)
    }

    fn non_stub_path(&self, file_id: &FileId) -> Option<&Arc<PathBuf>> {
        self.non_stub_paths.get_by_right(file_id)
    }

    fn non_stub_file_id(&self, path: &PathBuf) -> Option<&FileId> {
        self.non_stub_paths.get_by_left(path)
    }
}

#[derive(Debug, Clone)]
pub struct BuiltinSymbolTable {
    table: SymbolTable,
    parsed_file: Parsed<ModModule>,
    path: Arc<PathBuf>,
}

impl BuiltinSymbolTable {
    pub fn path(&self) -> &Arc<PathBuf> {
        &self.path
    }

    pub fn suite(&self) -> &Suite {
        self.parsed_file.suite()
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

    pub fn lookup_symbol(&self, name: &str, scope_id: ScopeId) -> Option<(SymbolId, &Symbol)> {
        self.table
            .lookup_symbol(name, scope_id)
            .filter(|(_, symbol)| {
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
            parsed_file: Parsed::default(),
            path: Arc::new(PathBuf::new()),
        }
    }
}

type SymbolTables = FxHashMap<FileId, SymbolTable>;
type ParsedFiles = FxHashMap<FileId, Parsed<ModModule>>;

#[derive(Debug, Clone)]
pub struct Indexer {
    tables: SymbolTables,
    parsed_files: ParsedFiles,
    files: Files,
    builtin_symbol_table: BuiltinSymbolTable,

    exec_env: ExecutionEnvironment,
    config: Config,
    host: PythonHost,

    collection_stub_path: Arc<PathBuf>,
    builtins_stub_path: Arc<PathBuf>,

    import_resolver_cache: Arc<RwLock<ImportResolverCache>>,
    /// For logging purposes
    total_indexed_files: usize,
}

impl Indexer {
    pub(crate) fn new(root: PathBuf, python_host: PythonHost) -> Self {
        let typeshed_path = sith_vendored::setup_typeshed();
        Self {
            tables: FxHashMap::default(),
            parsed_files: FxHashMap::default(),
            files: Files::default(),
            builtin_symbol_table: BuiltinSymbolTable::default(),
            collection_stub_path: Arc::new(typeshed_path.join("stdlib/collections/__init__.pyi")),
            builtins_stub_path: Arc::new(typeshed_path.join("stdlib/builtins.pyi")),
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
            import_resolver_cache: Arc::new(RwLock::new(ImportResolverCache::default())),
            total_indexed_files: 0,
        }
    }

    pub(crate) fn with_builtin_symbols(mut self) -> Self {
        let source = std::fs::read_to_string(self.builtins_stub_path.as_path())
            .expect("builtins.pyi file not found");

        let file_id = self.files.get_or_assign_id(self.builtins_stub_path.clone());
        let (table, parsed_file, _) = Indexer::index_content(
            &source,
            file_id,
            &self.builtins_stub_path,
            self.import_resolver_config(),
            self.import_resolver_cache.clone(),
        );

        self.builtin_symbol_table = BuiltinSymbolTable {
            table,
            parsed_file,
            path: self.builtins_stub_path.clone(),
        };

        self
    }

    pub(crate) fn with_collection_types(mut self) -> Self {
        let source = std::fs::read_to_string(self.collection_stub_path.as_path())
            .expect("collections/__init__.pyi file not found");

        let file_id = self
            .files
            .get_or_assign_id(self.collection_stub_path.clone());
        let (table, parsed_file, _) = Indexer::index_content(
            &source,
            file_id,
            &self.builtins_stub_path,
            self.import_resolver_config(),
            self.import_resolver_cache.clone(),
        );
        self.tables.insert(file_id, table);
        self.parsed_files.insert(file_id, parsed_file);

        self
    }

    fn import_resolver_config(&self) -> ImportResolverConfig {
        ImportResolverConfig::new(&self.exec_env, &self.config, &self.host)
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
    fn discover_python_files(root_dir: impl AsRef<Path>) -> Vec<PathBuf> {
        let root_dir = root_dir.as_ref();
        assert!(root_dir.is_dir());

        let mut python_files = Vec::new();

        let should_scan_subdirs = std::fs::read_dir(root_dir)
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
        source: &str,
        file_id: FileId,
        filepath: &Path,
        resolver_config: ImportResolverConfig,
        resolver_cache: Arc<RwLock<ImportResolverCache>>,
    ) -> (SymbolTable, Parsed<ModModule>, Vec<ImportSource>) {
        let parsed_file = parse_module(source);
        let table = SymbolTableBuilder::new(filepath, file_id, resolver_config, resolver_cache)
            .build(parsed_file.suite());
        let import_sources = table
            .declarations()
            .filter_map(|declaration| declaration.import_source())
            .filter(|import_source| !import_source.is_unresolved())
            .cloned()
            .collect();

        (table, parsed_file, import_sources)
    }

    pub(crate) fn index(mut self) -> Self {
        let python_files = Indexer::discover_python_files(&self.exec_env.root)
            .into_iter()
            .map(Arc::new);
        let parsed_files = Arc::new(Mutex::new(FxHashMap::with_capacity_and_hasher(
            python_files.len(),
            FxBuildHasher,
        )));
        let tables = Arc::new(Mutex::new(FxHashMap::with_capacity_and_hasher(
            python_files.len(),
            FxBuildHasher,
        )));
        let files = Arc::new(Mutex::new(self.files.clone()));

        let now = Instant::now();
        self.index_files(
            python_files,
            tables.clone(),
            parsed_files.clone(),
            files.clone(),
        );
        let elapsed = now.elapsed();
        tracing::info!(
            "Indexed {} files in {:?}",
            self.total_indexed_files,
            elapsed
        );

        self.tables
            .extend(Arc::into_inner(tables).unwrap().into_inner().unwrap());
        self.parsed_files
            .extend(Arc::into_inner(parsed_files).unwrap().into_inner().unwrap());

        self.files = Arc::into_inner(files).unwrap().into_inner().unwrap();

        self
    }

    fn index_files(
        &mut self,
        files_to_index: impl Iterator<Item = Arc<PathBuf>>,
        tables: Arc<Mutex<SymbolTables>>,
        parsed_files: Arc<Mutex<ParsedFiles>>,
        files: Arc<Mutex<Files>>,
    ) {
        let mut total_indexed_files = 0;
        let import_resolver_config = self.import_resolver_config();
        let files_to_index = Arc::new(Mutex::new(FxHashSet::from_iter(files_to_index)));

        loop {
            let mut files_to_process = Vec::new();
            {
                let mut files_to_index = files_to_index.lock().unwrap();
                if files_to_index.is_empty() {
                    break;
                }

                let mut files = files.lock().unwrap();
                for path in files_to_index.drain().filter(|path| {
                    !util::is_path_dir(path.as_path())
                        && path
                            .extension()
                            .is_some_and(|ext| ext != "so" && ext != "pyd")
                }) {
                    if files.indexed.insert(path.clone()) {
                        files_to_process.push(path);
                    }
                }
            }

            total_indexed_files += files_to_process.len();

            {
                let parsed_files = Arc::clone(&parsed_files);
                let tables = Arc::clone(&tables);
                let files = Arc::clone(&files);
                let files_to_index = Arc::clone(&files_to_index);
                let import_resolver_cache = Arc::clone(&self.import_resolver_cache);

                rayon::scope(move |scope| {
                    for file in files_to_process {
                        let parsed_files = Arc::clone(&parsed_files);
                        let tables = Arc::clone(&tables);
                        let files = Arc::clone(&files);
                        let files_to_index = Arc::clone(&files_to_index);
                        let import_resolver_cache = Arc::clone(&import_resolver_cache);

                        scope.spawn(move |_| {
                            let Ok(source) = util::read_to_string(file.as_path()).map_err(|err| {
                                tracing::error!("Failed to read `{}`: {err}", file.display());
                            }) else {
                                return;
                            };
                            let file_id = {
                                let mut files = files.lock().unwrap();
                                files.get_or_assign_id(file.clone())
                            };

                            let (table, parsed_file, import_sources) = Indexer::index_content(
                                &source,
                                file_id,
                                &file,
                                import_resolver_config,
                                import_resolver_cache,
                            );

                            {
                                let mut files = files.lock().unwrap();
                                let mut files_to_index = files_to_index.lock().unwrap();
                                for import_source in import_sources {
                                    // SAFETY: `import_sources` was filtered with only
                                    // resolved imports in `Indexer::index_content`.
                                    files_to_index
                                        .insert(import_source.any_path().unwrap().clone());
                                    files.push_source(import_source);
                                }
                            }

                            tables.lock().unwrap().insert(file_id, table);
                            parsed_files.lock().unwrap().insert(file_id, parsed_file);
                        });
                    }
                });
            }
        }

        self.total_indexed_files = total_indexed_files;
    }

    pub fn add_or_update_file(&mut self, filepath: PathBuf, source: Source) {
        let file = Arc::new(filepath);
        let (file_id, source) = match source {
            Source::New(source) => {
                self.files.indexed.insert(file.clone());
                let file_id = self.files.get_or_assign_id(file.clone());
                (file_id, source)
            }
            Source::Update { new, .. } => (self.file_id(&file), new),
        };

        let resolver_config = ImportResolverConfig::new(&self.exec_env, &self.config, &self.host);
        let (table, parsed_file, import_sources) = Indexer::index_content(
            source,
            file_id,
            &file,
            resolver_config,
            self.import_resolver_cache.clone(),
        );

        self.tables.insert(file_id, table);
        self.parsed_files.insert(file_id, parsed_file);

        let parsed_files = Arc::new(Mutex::new(FxHashMap::default()));
        let tables = Arc::new(Mutex::new(FxHashMap::default()));
        let files = Arc::new(Mutex::new(std::mem::take(&mut self.files)));

        self.index_files(
            import_sources
                .into_iter()
                .filter_map(|source| source.any_path().map(Clone::clone)),
            tables.clone(),
            parsed_files.clone(),
            files.clone(),
        );

        self.tables
            .extend(Arc::into_inner(tables).unwrap().into_inner().unwrap());
        self.parsed_files
            .extend(Arc::into_inner(parsed_files).unwrap().into_inner().unwrap());
        self.files = Arc::into_inner(files).unwrap().into_inner().unwrap();
    }

    pub fn build_symbol_table(
        &self,
        path: &Path,
        file_id: FileId,
        content: &str,
    ) -> (SymbolTable, Parsed<ModModule>) {
        let parsed_file = parse_module(content);
        (
            SymbolTableBuilder::new(
                path,
                file_id,
                self.import_resolver_config(),
                self.import_resolver_cache.clone(),
            )
            .build(parsed_file.suite()),
            parsed_file,
        )
    }

    pub(crate) fn table(&self, file_id: &FileId) -> Option<&SymbolTable> {
        self.tables.get(file_id)
    }

    pub(crate) fn builtin_symbol_table(&self) -> &BuiltinSymbolTable {
        &self.builtin_symbol_table
    }

    pub(crate) fn collection_stub_path(&self) -> &PathBuf {
        &self.collection_stub_path
    }

    pub fn tables_iter(&self) -> impl Iterator<Item = (&FileId, &SymbolTable)> {
        self.tables.iter()
    }

    pub fn typeshed_path(&self) -> &PathBuf {
        self.config.typeshed_path.as_ref().unwrap()
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
    pub fn file_path(&self, file_id: &FileId) -> &Arc<PathBuf> {
        self.files
            .any_path(file_id)
            .unwrap_or_else(|| panic!("no file path for {:?}", file_id))
    }

    pub fn non_stub_path(&self, file_id: &FileId) -> Option<&Arc<PathBuf>> {
        self.files.non_stub_path(file_id)
    }

    /// Returns the [`FileId`] for a given `file_path`.
    /// # Panics
    /// If there's no [`FileId`] for a `file_path`.
    pub fn file_id(&self, file_path: &PathBuf) -> FileId {
        self.files
            .any_file_id(file_path)
            .unwrap_or_else(|| panic!("no `file_id` for {}", file_path.display()))
    }

    /// Returns the AST for the `file_path`, if `file_path` was not indexed
    /// returns `None`.
    pub fn ast(&self, file_path: &PathBuf) -> Option<&Parsed<ModModule>> {
        Some(
            match self
                .files
                .any_file_id(file_path)
                .and_then(|file_id| self.parsed_files.get(&file_id))
            {
                Some(ast) => ast,
                None if file_path.ends_with("stdlib/builtins.pyi") => {
                    &self.builtin_symbol_table.parsed_file
                }
                _ => return None,
            },
        )
    }

    /// Returns the AST for the `file_path`.
    ///
    /// # Panics
    /// If `file_path` was not indexed.
    pub fn ast_or_panic(&self, file_path: &PathBuf) -> &Parsed<ModModule> {
        self.ast(file_path)
            .unwrap_or_else(|| panic!("no ast for path: {}", file_path.display()))
    }

    pub fn node_stack(&self, file: &PathBuf) -> NodeStack {
        let suite = self.ast_or_panic(file).suite();
        NodeStack::default().build(suite)
    }

    pub fn is_indexed(&self, path: &PathBuf) -> bool {
        self.files.indexed.contains(path)
    }
}
