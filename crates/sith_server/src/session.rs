//! Data model, state management, and configuration resolution.

mod capabilities;
mod settings;
mod workspace;

use std::sync::Arc;

use anyhow::anyhow;
use lsp_types::{ClientCapabilities, Url};
use semantic_model::db::Source;
use settings::ResolvedClientSettings;
use workspace::{DocumentController, DocumentRef, SymbolTableDbRef, Workspaces};

use crate::edit::DocumentVersion;
use crate::PositionEncoding;

pub(crate) use self::capabilities::ResolvedClientCapabilities;
pub(crate) use self::settings::{AllSettings, ClientSettings};

/// The global state for the LSP
pub(crate) struct Session {
    /// Workspace folders in the current session, which contain the state of all open files.
    workspaces: Workspaces,
    /// The global position encoding, negotiated during LSP initialization.
    position_encoding: PositionEncoding,
    /// Global settings provided by the client.
    global_settings: ClientSettings,
    /// Tracks what LSP features the client supports and doesn't support.
    resolved_client_capabilities: Arc<ResolvedClientCapabilities>,
}

/// An immutable snapshot of `Session` that references
/// a specific document.
pub(crate) struct DocumentSnapshot {
    client_settings: ResolvedClientSettings,
    resolved_client_capabilities: Arc<ResolvedClientCapabilities>,
    symbol_table_db_ref: SymbolTableDbRef,
    document_ref: DocumentRef,
    position_encoding: PositionEncoding,
    url: Url,
}

impl Session {
    pub(crate) fn new(
        client_capabilities: &ClientCapabilities,
        position_encoding: PositionEncoding,
        global_settings: ClientSettings,
        workspaces: Vec<(Url, ClientSettings)>,
    ) -> crate::Result<Self> {
        Ok(Self {
            position_encoding,
            resolved_client_capabilities: Arc::new(ResolvedClientCapabilities::new(
                client_capabilities,
            )),
            workspaces: Workspaces::new(workspaces, global_settings.clone())?,
            global_settings,
        })
    }

    pub(crate) fn take_snapshot(&self, url: &Url) -> Option<DocumentSnapshot> {
        Some(DocumentSnapshot {
            client_settings: self.workspaces.client_settings(url, &self.global_settings),
            resolved_client_capabilities: self.resolved_client_capabilities.clone(),
            document_ref: self.workspaces.document_snapshot(url)?,
            symbol_table_db_ref: self.workspaces.db_snapshot(url)?,
            position_encoding: self.position_encoding,
            url: url.clone(),
        })
    }

    pub(crate) fn open_document(&mut self, url: &Url, contents: String, version: DocumentVersion) {
        self.workspaces.open(url, contents, version);
    }

    pub(crate) fn close_document(&mut self, url: &Url) -> crate::Result<()> {
        self.workspaces.close(url)?;
        Ok(())
    }

    pub(crate) fn document_controller(
        &mut self,
        url: &Url,
    ) -> crate::Result<&mut DocumentController> {
        self.workspaces
            .controller(url)
            .ok_or_else(|| anyhow!("Tried to open unavailable document `{url}`"))
    }

    pub(crate) fn update_document(&mut self, url: &Url, content: Source) -> crate::Result<()> {
        self.workspaces
            .update_ast(url, content)
            .map(|_| anyhow!("Tried to open unavailable document `{url}`"))?;

        Ok(())
    }

    pub(crate) fn open_workspace_folder(&mut self, url: &Url) -> crate::Result<()> {
        self.workspaces
            .open_workspace_folder(url, self.global_settings.clone())?;
        Ok(())
    }

    pub(crate) fn close_workspace_folder(&mut self, url: &Url) -> crate::Result<()> {
        self.workspaces.close_workspace_folder(url)?;
        Ok(())
    }

    // TODO: use this function when figure it out how to implement workspace/didChangeConfiguration
    #[allow(dead_code)]
    pub(crate) fn update_workspace_settings(
        &mut self,
        url: &Url,
        settings: serde_json::Value,
    ) -> crate::Result<()> {
        self.workspaces
            .update_workspace_settings(url, ClientSettings::from_value(settings))?;
        Ok(())
    }

    pub(crate) fn resolved_client_capabilities(&self) -> &ResolvedClientCapabilities {
        &self.resolved_client_capabilities
    }

    pub(crate) fn encoding(&self) -> PositionEncoding {
        self.position_encoding
    }
}

impl DocumentSnapshot {
    pub(crate) fn resolved_client_capabilities(&self) -> &ResolvedClientCapabilities {
        &self.resolved_client_capabilities
    }

    pub(crate) fn db(&self) -> &SymbolTableDbRef {
        &self.symbol_table_db_ref
    }

    pub(crate) fn document(&self) -> &DocumentRef {
        &self.document_ref
    }

    pub(crate) fn encoding(&self) -> PositionEncoding {
        self.position_encoding
    }

    pub(crate) fn url(&self) -> &Url {
        &self.url
    }

    pub(crate) fn document_version(&self) -> i32 {
        self.document_ref.version()
    }

    pub(crate) fn client_settings(&self) -> &ResolvedClientSettings {
        &self.client_settings
    }
}
