use std::{ops::Deref, path::PathBuf};

use lsp_types::{ClientCapabilities, Url};
use rustc_hash::FxHashMap;
use serde::Deserialize;

/// Maps a workspace URI to its associated client settings. Used during server initialization.
pub(crate) type WorkspaceSettingsMap = FxHashMap<Url, ClientSettings>;

#[derive(Debug, Deserialize, Default, Clone)]
#[cfg_attr(test, derive(PartialEq, Eq))]
#[serde(rename_all = "camelCase")]
pub(crate) struct ClientSettings {
    interpreter: Option<String>,
    ruff: Option<RuffOptions>,

    // These settings are only needed for tracing, and are only read from the global configuration.
    // These will not be in the resolved settings.
    #[serde(flatten)]
    pub(crate) tracing: TracingSettings,
}

/// Settings needed to initialize tracing. These will only be
/// read from the global configuration.
#[derive(Debug, Deserialize, Default, Clone)]
#[cfg_attr(test, derive(PartialEq, Eq))]
#[serde(rename_all = "camelCase")]
pub(crate) struct TracingSettings {
    pub(crate) log_level: Option<crate::trace::LogLevel>,
    /// Path to the log file - tildes and environment variables are supported.
    pub(crate) log_file: Option<PathBuf>,
}

/// This is a direct representation of the workspace settings schema,
/// which inherits the schema of [`ClientSettings`] and adds extra fields
/// to describe the workspace it applies to.
#[derive(Debug, Deserialize)]
#[cfg_attr(test, derive(PartialEq, Eq))]
#[serde(rename_all = "camelCase")]
struct WorkspaceSettings {
    #[serde(flatten)]
    settings: ClientSettings,
    workspace: String,
}

#[derive(Debug, Deserialize, Default, Clone)]
#[cfg_attr(test, derive(PartialEq, Eq))]
#[serde(rename_all = "camelCase")]
pub(crate) struct RuffOptions {
    path: Option<String>,
    format: Option<FormatOptions>,
    lint: Option<LintOptions>,
}

#[derive(Debug, Deserialize, Default, Clone)]
#[cfg_attr(test, derive(PartialEq, Eq))]
#[serde(rename_all = "camelCase")]
pub(crate) struct LintOptions {
    enable: Option<bool>,
    select: Option<Vec<String>>,
    extend_select: Option<Vec<String>>,
    ignore: Option<Vec<String>>,
    args: Option<Vec<String>>,
}

#[derive(Debug, Deserialize, Default, Clone)]
#[cfg_attr(test, derive(PartialEq, Eq))]
#[serde(rename_all = "camelCase")]
pub(crate) struct FormatOptions {
    enable: Option<bool>,
    args: Option<Vec<String>>,
}

/// This is the exact schema for initialization options sent in by the client
/// during initialization.
#[derive(Debug, Deserialize)]
#[cfg_attr(test, derive(PartialEq, Eq))]
#[serde(untagged)]
enum InitializationOptions {
    #[serde(rename_all = "camelCase")]
    HasWorkspaces {
        global_settings: ClientSettings,
        #[serde(rename = "settings")]
        workspace_settings: Vec<WorkspaceSettings>,
    },
    GlobalOnly {
        #[serde(default)]
        settings: ClientSettings,
    },
}

impl Default for InitializationOptions {
    fn default() -> Self {
        Self::GlobalOnly {
            settings: ClientSettings::default(),
        }
    }
}

/// Resolved client settings for a specific document. These settings are meant to be
/// used directly by the server, and are *not* a 1:1 representation with how the client
/// sends them.
#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq, Eq))]
#[allow(clippy::struct_excessive_bools)]
pub(crate) struct ResolvedClientSettings {
    interpreter: Option<String>,
    ruff_path: Option<String>,
    format_enable: bool,
    format_args: Vec<String>,
    lint_enable: bool,
    lint_select: Option<Vec<String>>,
    lint_extend_select: Option<Vec<String>>,
    lint_ignore: Option<Vec<String>>,
    lint_args: Vec<String>,
}

/// Built from the initialization options provided by the client.
#[derive(Debug)]
pub(crate) struct AllSettings {
    pub(crate) global_settings: ClientSettings,
    /// If this is `None`, the client only passed in global settings.
    pub(crate) workspace_settings: Option<WorkspaceSettingsMap>,
}

impl AllSettings {
    /// Initializes the controller from the serialized initialization options.
    /// This fails if `options` are not valid initialization options.
    pub(crate) fn from_value(options: serde_json::Value) -> Self {
        Self::from_init_options(
            serde_json::from_value(options)
                .map_err(|err| {
                    tracing::error!("Failed to deserialize initialization options: {err}. Falling back to default client settings...");
                    // show_err_msg!("SithLSP received invalid client settings - falling back to default client settings.");
                })
                .unwrap_or_default(),
        )
    }

    fn from_init_options(options: InitializationOptions) -> Self {
        let (global_settings, workspace_settings) = match options {
            InitializationOptions::GlobalOnly { settings } => (settings, None),
            InitializationOptions::HasWorkspaces {
                global_settings,
                workspace_settings,
            } => (global_settings, Some(workspace_settings)),
        };

        Self {
            global_settings,
            workspace_settings: workspace_settings.map(|workspace_settings| {
                workspace_settings
                    .into_iter()
                    .map(|settings| {
                        (
                            Url::from_file_path(&settings.workspace).unwrap(),
                            settings.settings,
                        )
                    })
                    .collect()
            }),
        }
    }
}

impl ClientSettings {
    pub(crate) fn from_value(value: serde_json::Value) -> Self {
        serde_json::from_value(value)
                .map_err(|err| {
                    tracing::error!("Failed to deserialize client settings: {err}. Falling back to default client settings...");
                    // show_err_msg!("SithLSP received invalid client settings - falling back to default client settings.");
                })
                .unwrap_or_default()
    }
}

impl ResolvedClientSettings {
    /// Resolves a series of client settings, prioritizing workspace settings over global settings.
    /// Any fields not specified by either are set to their defaults.
    pub(super) fn with_workspace(
        workspace_settings: &ClientSettings,
        global_settings: &ClientSettings,
    ) -> Self {
        Self::new_impl(&[workspace_settings, global_settings])
    }

    /// Resolves global settings only.
    pub(super) fn global(global_settings: &ClientSettings) -> Self {
        Self::new_impl(&[global_settings])
    }

    fn new_impl(all_settings: &[&ClientSettings]) -> Self {
        Self {
            interpreter: Self::resolve_optional(all_settings, |settings| {
                settings.interpreter.clone()
            }),
            ruff_path: Self::resolve_optional(all_settings, |settings| {
                settings.ruff.as_ref()?.path.clone()
            }),
            format_enable: Self::resolve_or(
                all_settings,
                |settings| settings.ruff.as_ref()?.format.as_ref()?.enable,
                true,
            ),
            format_args: Self::resolve_or(
                all_settings,
                |settings| settings.ruff.as_ref()?.format.as_ref()?.args.clone(),
                Vec::new(),
            ),
            lint_enable: Self::resolve_or(
                all_settings,
                |settings| settings.ruff.as_ref()?.lint.as_ref()?.enable,
                true,
            ),
            lint_select: Self::resolve_optional(all_settings, |settings| {
                settings.ruff.as_ref()?.lint.as_ref()?.select.clone()
            }),
            lint_extend_select: Self::resolve_optional(all_settings, |settings| {
                settings.ruff.as_ref()?.lint.as_ref()?.extend_select.clone()
            }),
            lint_ignore: Self::resolve_optional(all_settings, |settings| {
                settings.ruff.as_ref()?.lint.as_ref()?.ignore.clone()
            }),
            lint_args: Self::resolve_or(
                all_settings,
                |settings| settings.ruff.as_ref()?.lint.as_ref()?.args.clone(),
                Vec::new(),
            ),
        }
    }

    /// Attempts to resolve a setting using a list of available client settings as sources.
    /// Client settings that come earlier in the list take priority. This function is for fields
    /// that do not have a default value and should be left unset.
    /// Use [`ResolvedClientSettings::resolve_or`] for settings that should have default values.
    fn resolve_optional<T>(
        all_settings: &[&ClientSettings],
        get: impl Fn(&ClientSettings) -> Option<T>,
    ) -> Option<T> {
        all_settings.iter().map(Deref::deref).find_map(get)
    }

    /// Attempts to resolve a setting using a list of available client settings as sources.
    /// Client settings that come earlier in the list take priority. `default` will be returned
    /// if none of the settings specify the requested setting.
    /// Use [`ResolvedClientSettings::resolve_optional`] if the setting should be optional instead
    /// of having a default value.
    fn resolve_or<T>(
        all_settings: &[&ClientSettings],
        get: impl Fn(&ClientSettings) -> Option<T>,
        default: T,
    ) -> T {
        Self::resolve_optional(all_settings, get).unwrap_or(default)
    }
}

impl ResolvedClientSettings {
    pub(crate) fn ruff_path(&self) -> Option<&String> {
        self.ruff_path.as_ref()
    }

    pub(crate) fn is_format_enabled(&self) -> bool {
        self.format_enable
    }

    pub(crate) fn format_args(&self) -> &[String] {
        &self.format_args
    }

    pub(crate) fn interpreter(&self) -> Option<&String> {
        self.interpreter.as_ref()
    }

    pub(crate) fn is_lint_enabled(&self) -> bool {
        self.lint_enable
    }

    pub(crate) fn lint_args(&self) -> &[String] {
        &self.lint_args
    }

    pub(crate) fn lint_select(&self) -> Option<&Vec<String>> {
        self.lint_select.as_ref()
    }

    pub(crate) fn lint_extend_select(&self) -> Option<&Vec<String>> {
        self.lint_extend_select.as_ref()
    }

    pub(crate) fn lint_ignore(&self) -> Option<&Vec<String>> {
        self.lint_ignore.as_ref()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub(crate) struct ResolvedClientCapabilities {
    pub(crate) code_action_deferred_edit_resolution: bool,
    pub(crate) pull_diagnostics: bool,
    pub(crate) completion_item_resolve_properties_support: Vec<String>,
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

        Self {
            code_action_deferred_edit_resolution: code_action_data_support
                && code_action_edit_resolution,
            pull_diagnostics,
            completion_item_resolve_properties_support,
        }
    }
}

#[cfg(test)]
mod tests {
    use insta::assert_debug_snapshot;
    use serde::de::DeserializeOwned;

    use super::*;

    #[cfg(not(windows))]
    const VS_CODE_INIT_OPTIONS_FIXTURE: &str =
        include_str!("../../resources/test/fixtures/settings/vs_code_initialization_options.json");
    const GLOBAL_ONLY_INIT_OPTIONS_FIXTURE: &str =
        include_str!("../../resources/test/fixtures/settings/global_only.json");
    const EMPTY_INIT_OPTIONS_FIXTURE: &str =
        include_str!("../../resources/test/fixtures/settings/empty.json");

    fn deserialize_fixture<T: DeserializeOwned>(content: &str) -> T {
        serde_json::from_str(content).expect("test fixture JSON should deserialize")
    }

    #[cfg(not(windows))]
    #[test]
    fn test_vs_code_init_options_deserialize() {
        let options: InitializationOptions = deserialize_fixture(VS_CODE_INIT_OPTIONS_FIXTURE);
        assert_debug_snapshot!(options);
    }

    #[test]
    fn test_global_only_resolves_correctly() {
        let options: InitializationOptions = deserialize_fixture(GLOBAL_ONLY_INIT_OPTIONS_FIXTURE);
        assert_debug_snapshot!(options);
    }

    #[test]
    fn test_empty_init_options_deserialize() {
        let options: InitializationOptions = deserialize_fixture(EMPTY_INIT_OPTIONS_FIXTURE);
        assert_eq!(options, InitializationOptions::default());
    }
}
