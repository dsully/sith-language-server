use anyhow::{anyhow, Context};
use lsp_types::Url;
use python_utils::{get_python_platform, get_python_search_paths, get_python_version};
use ruff_python_resolver::python_platform::PythonPlatform;
use ruff_python_resolver::python_version::PythonVersion;
use rustc_hash::FxHashMap;
use semantic_model::db::{Source, SymbolTableDb};
use std::collections::BTreeMap;
use std::ops::Deref;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use crate::edit::DocumentVersion;
use crate::Document;

use super::settings::ResolvedClientSettings;
use super::ClientSettings;

#[derive(Default)]
pub(crate) struct Workspaces(BTreeMap<PathBuf, Workspace>);

#[derive(Debug)]
pub(crate) struct Workspace {
    open_documents: OpenDocuments,
    symbol_table_db: SymbolTableDbController,
    settings: ClientSettings,
}

#[derive(Default, Debug)]
pub(crate) struct OpenDocuments {
    documents: FxHashMap<Url, DocumentController>,
}

/// A mutable handler to an underlying document.
/// Handles copy-on-write mutation automatically when
/// calling `deref_mut`.
#[derive(Debug)]
pub(crate) struct DocumentController {
    document: Arc<Document>,
}

/// A read-only reference to a document.
#[derive(Clone)]
pub(crate) struct DocumentRef {
    document: Arc<Document>,
}

#[derive(Debug)]
pub(crate) struct SymbolTableDbController {
    table: Arc<SymbolTableDb>,
}

impl SymbolTableDbController {
    pub fn new(
        root: PathBuf,
        python_version: PythonVersion,
        python_platform: PythonPlatform,
        python_search_paths: Vec<PathBuf>,
    ) -> Self {
        Self {
            table: Arc::new(
                SymbolTableDb::new(root, python_version, python_platform, python_search_paths)
                    .with_builtin_symbols(),
            ),
        }
    }

    pub fn make_ref(&self) -> SymbolTableDbRef {
        SymbolTableDbRef {
            table: self.table.clone(),
        }
    }

    pub fn make_mut(&mut self) -> &mut SymbolTableDb {
        Arc::make_mut(&mut self.table)
    }
}

impl Deref for SymbolTableDbController {
    type Target = SymbolTableDb;
    fn deref(&self) -> &Self::Target {
        &self.table
    }
}

#[derive(Clone)]
pub(crate) struct SymbolTableDbRef {
    table: Arc<SymbolTableDb>,
}

impl Deref for SymbolTableDbRef {
    type Target = SymbolTableDb;
    fn deref(&self) -> &Self::Target {
        &self.table
    }
}

impl Workspace {
    pub(crate) fn new(root: &Url, settings: ClientSettings) -> crate::Result<(PathBuf, Self)> {
        let path = root
            .to_file_path()
            .map_err(|_| anyhow!("workspace URL was not a file path!"))?;

        let resolved_settings =
            ResolvedClientSettings::with_workspace(&settings, &ClientSettings::default());
        let Some(interpreter) = resolved_settings.interpreter() else {
            return Err(anyhow!("Python interpreter path not set!"));
        };
        let search_paths = get_python_search_paths(interpreter);
        let python_version = get_python_version(interpreter).map_err(|err| anyhow!(err))?;
        let python_platform = get_python_platform().map_err(|err| anyhow!(err))?;

        Ok((
            path.clone(),
            Self {
                open_documents: OpenDocuments::default(),
                symbol_table_db: SymbolTableDbController::new(
                    path,
                    python_version,
                    python_platform,
                    search_paths.paths,
                ),
                settings,
            },
        ))
    }
}

impl Workspaces {
    pub(super) fn new(urls: Vec<(Url, ClientSettings)>) -> crate::Result<Self> {
        Ok(Self(
            urls.into_iter()
                .map(|(url, settings)| Workspace::new(&url, settings))
                .collect::<crate::Result<_>>()?,
        ))
    }

    pub(super) fn open_workspace_folder(
        &mut self,
        folder_url: &Url,
        settings: ClientSettings,
    ) -> crate::Result<()> {
        let (path, workspace) = Workspace::new(folder_url, settings)?;
        self.0.insert(path, workspace);
        Ok(())
    }

    pub(super) fn close_workspace_folder(&mut self, folder_url: &Url) -> crate::Result<()> {
        let path = folder_url
            .to_file_path()
            .map_err(|()| anyhow!("Folder URI was not a proper file path"))?;
        self.0
            .remove(&path)
            .ok_or_else(|| anyhow!("Tried to remove non-existent folder {}", path.display()))?;
        Ok(())
    }

    pub(super) fn update_workspace_settings(
        &mut self,
        folder_url: &Url,
        client_settings: ClientSettings,
    ) -> crate::Result<()> {
        let workspace = self
            .workspace_for_url_mut(folder_url)
            .context(format!("Failed to get workspace `{folder_url}`"))?;
        workspace.settings = client_settings;
        Ok(())
    }

    pub(super) fn document_snapshot(&self, document_url: &Url) -> Option<DocumentRef> {
        self.workspace_for_url(document_url)?
            .open_documents
            .snapshot(document_url)
    }

    pub(super) fn db_snapshot(&self, document_url: &Url) -> Option<SymbolTableDbRef> {
        Some(
            self.workspace_for_url(document_url)?
                .symbol_table_db
                .make_ref(),
        )
    }

    pub(super) fn controller(&mut self, document_url: &Url) -> Option<&mut DocumentController> {
        self.workspace_for_url_mut(document_url)?
            .open_documents
            .controller(document_url)
    }

    pub(super) fn update_ast(&mut self, document_url: &Url, content: Source) -> crate::Result<()> {
        let db = self
            .workspace_for_url_mut(document_url)
            .context(format!(
                "Failed to get workspace for document `{document_url}`"
            ))?
            .symbol_table_db
            .make_mut();

        let file_path = document_url
            .to_file_path()
            .expect("Url is not filepath like");
        db.indexer_mut().add_or_update_file(file_path, content);

        Ok(())
    }

    pub(super) fn open(&mut self, url: &Url, contents: String, version: DocumentVersion) {
        if let Some(workspace) = self.workspace_for_url_mut(url) {
            let db = workspace.symbol_table_db.make_mut();
            db.indexer_mut().add_or_update_file(
                url.to_file_path().expect("Url is not filepath like"),
                Source::New(&contents),
            );
            workspace.open_documents.open(url, contents, version);
        }
    }

    pub(super) fn close(&mut self, url: &Url) -> crate::Result<()> {
        self.workspace_for_url_mut(url)
            .ok_or_else(|| anyhow!("Workspace not found for {url}"))?
            .open_documents
            .close(url)
    }

    pub(super) fn client_settings(
        &self,
        url: &Url,
        global_settings: &ClientSettings,
    ) -> ResolvedClientSettings {
        self.workspace_for_url(url).map_or_else(
            || {
                tracing::warn!(
                    "Workspace not found for {url}. Global settings will be used for this document"
                );
                ResolvedClientSettings::global(global_settings)
            },
            |workspace| {
                ResolvedClientSettings::with_workspace(&workspace.settings, global_settings)
            },
        )
    }

    fn workspace_for_url(&self, url: &Url) -> Option<&Workspace> {
        Some(self.entry_for_url(url)?.1)
    }

    fn workspace_for_url_mut(&mut self, url: &Url) -> Option<&mut Workspace> {
        Some(self.entry_for_url_mut(url)?.1)
    }

    fn entry_for_url(&self, url: &Url) -> Option<(&Path, &Workspace)> {
        let path = url.to_file_path().ok()?;
        self.0
            .range(..path)
            .next_back()
            .map(|(path, workspace)| (path.as_path(), workspace))
    }

    fn entry_for_url_mut(&mut self, url: &Url) -> Option<(&Path, &mut Workspace)> {
        let path = url.to_file_path().ok()?;
        self.0
            .range_mut(..path)
            .next_back()
            .map(|(path, workspace)| (path.as_path(), workspace))
    }
}

impl OpenDocuments {
    fn snapshot(&self, url: &Url) -> Option<DocumentRef> {
        Some(self.documents.get(url)?.make_ref())
    }

    fn controller(&mut self, url: &Url) -> Option<&mut DocumentController> {
        self.documents.get_mut(url)
    }

    fn open(&mut self, url: &Url, contents: String, version: DocumentVersion) {
        if self
            .documents
            .insert(url.clone(), DocumentController::new(contents, version))
            .is_some()
        {
            tracing::warn!("Opening document `{url}` that is already open!");
        }
    }

    fn close(&mut self, url: &Url) -> crate::Result<()> {
        let Some(_) = self.documents.remove(url) else {
            return Err(anyhow!(
                "Tried to close document `{url}`, which was not open"
            ));
        };
        Ok(())
    }
}

impl DocumentController {
    fn new(contents: String, version: DocumentVersion) -> Self {
        Self {
            document: Arc::new(Document::new(contents, version)),
        }
    }

    pub(crate) fn make_ref(&self) -> DocumentRef {
        DocumentRef {
            document: self.document.clone(),
        }
    }

    pub(crate) fn make_mut(&mut self) -> &mut Document {
        Arc::make_mut(&mut self.document)
    }
}

impl Deref for DocumentController {
    type Target = Document;
    fn deref(&self) -> &Self::Target {
        &self.document
    }
}

impl Deref for DocumentRef {
    type Target = Document;
    fn deref(&self) -> &Self::Target {
        &self.document
    }
}
