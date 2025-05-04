use std::path::{Path, PathBuf};

use rustc_hash::FxHashMap;

use crate::{
    execution_environment::ExecutionEnvironment, import_result::ImportResult,
    module_descriptor::ImportModuleDescriptor,
};

#[derive(Debug, Hash, PartialEq, Eq)]
struct ImportResultCacheKey(PathBuf, String);

impl ImportResultCacheKey {
    fn create(root: PathBuf, source_file: Option<&Path>, import_name: &str) -> Self {
        Self(
            root,
            format!(
                "{}-{import_name}",
                source_file.unwrap_or(Path::new("")).display()
            ),
        )
    }
}

#[derive(Debug, Default)]
pub struct ImportResolverCache {
    import_result: FxHashMap<ImportResultCacheKey, ImportResult>,
    python_search_paths: Vec<PathBuf>,
    typeshed_path: Option<PathBuf>,
    typeshed_stdlib_path: Option<PathBuf>,
    typeshed_stubs_path: Option<PathBuf>,
    typeshed_third_party_package_paths: Option<FxHashMap<String, Vec<PathBuf>>>,
}

impl ImportResolverCache {
    pub(crate) fn add_import_result(
        &mut self,
        source_file: &Path,
        import_name: &str,
        execution_environment: &ExecutionEnvironment,
        result: ImportResult,
    ) {
        let source_file = result.is_relative.then_some(source_file);
        self.import_result.insert(
            ImportResultCacheKey::create(
                execution_environment.root.clone(),
                source_file,
                import_name,
            ),
            result,
        );
    }

    pub(crate) fn get_import_result(
        &self,
        source_file: &Path,
        import_name: &str,
        module_descriptor: &ImportModuleDescriptor,
        execution_environment: &ExecutionEnvironment,
    ) -> Option<ImportResult> {
        let source_file = (module_descriptor.leading_dots > 0).then_some(source_file);
        self.import_result
            .get(&ImportResultCacheKey::create(
                execution_environment.root.clone(),
                source_file,
                import_name,
            ))
            .cloned()
    }

    pub(crate) fn add_python_search_paths(&mut self, search_paths: Vec<PathBuf>) {
        self.python_search_paths = search_paths;
    }

    pub(crate) fn get_python_search_paths(&self) -> Option<Vec<PathBuf>> {
        if self.python_search_paths.is_empty() {
            None
        } else {
            Some(self.python_search_paths.clone())
        }
    }

    pub(crate) fn get_typeshed_path(&self) -> Option<PathBuf> {
        self.typeshed_path.clone()
    }

    pub(crate) fn add_typeshed_path(&mut self, typeshed_root: PathBuf) {
        self.typeshed_path = Some(typeshed_root);
    }

    pub(crate) fn get_typeshed_stdlib_path(&self) -> Option<PathBuf> {
        self.typeshed_stdlib_path.clone()
    }

    pub(crate) fn add_typshed_stdlib_path(&mut self, typeshed_stdlib_path: PathBuf) {
        self.typeshed_stdlib_path = Some(typeshed_stdlib_path);
    }

    pub(crate) fn get_typeshed_stubs_path(&self) -> Option<PathBuf> {
        self.typeshed_stubs_path.clone()
    }

    pub(crate) fn add_typshed_stubs_path(&mut self, typeshed_stubs_path: PathBuf) {
        self.typeshed_stubs_path = Some(typeshed_stubs_path);
    }

    pub(crate) fn get_typeshed_third_party_package_paths(
        &self,
    ) -> Option<FxHashMap<String, Vec<PathBuf>>> {
        self.typeshed_third_party_package_paths.clone()
    }

    pub(crate) fn add_typshed_third_party_package_paths(
        &mut self,
        packages: FxHashMap<String, Vec<PathBuf>>,
    ) {
        self.typeshed_third_party_package_paths = Some(packages);
    }
}
