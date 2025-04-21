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
    pub(crate) python_search_paths: Vec<PathBuf>,
    pub(crate) typeshed_root: Option<PathBuf>,
    pub(crate) typeshed_stdlib_path: Option<PathBuf>,
    pub(crate) typeshed_stubs_path: Option<PathBuf>,
    pub(crate) typeshed_third_party_package_paths: Option<FxHashMap<String, Vec<PathBuf>>>,
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
}
