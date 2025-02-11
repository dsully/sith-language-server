use lsp_types::ClientCapabilities;

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub(crate) struct ResolvedClientCapabilities {
    pub(crate) code_action_deferred_edit_resolution: bool,
    pub(crate) pull_diagnostics: bool,
    pub(crate) completion_item_resolve_properties_support: Vec<String>,
    pub(crate) document_changes: bool,
}

impl ResolvedClientCapabilities {
    pub(super) fn new(client_capabilities: &ClientCapabilities) -> Self {
        let code_action_settings = client_capabilities
            .text_document
            .as_ref()
            .and_then(|doc_settings| doc_settings.code_action.as_ref());
        let code_action_data_support = code_action_settings
            .and_then(|code_action_settings| code_action_settings.data_support)
            .unwrap_or_default();
        let code_action_edit_resolution = code_action_settings
            .and_then(|code_action_settings| code_action_settings.resolve_support.as_ref())
            .is_some_and(|resolve_support| resolve_support.properties.contains(&"edit".into()));

        let completion_settings = client_capabilities
            .text_document
            .as_ref()
            .and_then(|doc_settings| doc_settings.completion.as_ref());
        let completion_item_resolve_properties_support = completion_settings
            .and_then(|completion_settings| completion_settings.completion_item.as_ref())
            .and_then(|completion_item_settings| completion_item_settings.resolve_support.as_ref())
            .map(|resolve_support| resolve_support.properties.clone())
            .unwrap_or_default();

        let pull_diagnostics = client_capabilities
            .text_document
            .as_ref()
            .and_then(|text_document| text_document.diagnostic.as_ref())
            .is_some();

        let document_changes = client_capabilities
            .workspace
            .as_ref()
            .and_then(|workspace| workspace.workspace_edit.as_ref())
            .and_then(|workspace_edit| workspace_edit.document_changes)
            .unwrap_or_default();

        Self {
            code_action_deferred_edit_resolution: code_action_data_support
                && code_action_edit_resolution,
            pull_diagnostics,
            completion_item_resolve_properties_support,
            document_changes,
        }
    }
}
