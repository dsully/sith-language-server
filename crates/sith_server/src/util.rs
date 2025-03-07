use std::path::{Path, PathBuf};

use lsp_types::Url;

mod ruff;
pub(crate) use ruff::{
    run_ruff_check, run_ruff_fix, run_ruff_format, UNSUPPORTED_CHECK_ARGS, UNSUPPORTED_FORMAT_ARGS,
};

/// Check if the [`Url`] is from a file that only exists in memory.
///
/// When performing LSP operations on in-memory buffers:
///
/// - Neovim sends the URI "file://" for all in-memory buffers. This means different
///   unnamed buffers all share the same URI.
///
/// - VSCode sends URIs like "untitled:Untitled-1" for in-memory files. VSCode makes it
///   easier to handle multiple untitled files by incrementing the number suffix
///   (e.g., Untitled-1, Untitled-2), giving each in-memory file a unique URI.
pub(crate) fn is_url_unnamed(url: &Url) -> bool {
    // TODO: check if on Windows, Neovim unnamed buffers also have a '/' path
    url.scheme() == "untitled" || url.path() == "/"
}

/// Converts the [`Url`] to a [`PathBuf`] while fixing the path if it's from an
/// in-memory buffer.
/// - For VSCode in-memory files this function appends a ".py" to the [`Url`]
///   path and joined with the `root_path`.
/// - For Neovim in-memory buffers this function returns the filename
///   "___Neovim__Unnamed_Buffer___.py" joined with the `root_path`.
pub(crate) fn convert_url_to_path(root_path: &Path, url: &Url) -> PathBuf {
    if url.scheme() == "untitled" {
        root_path.join(format!("{}.py", url.path()))
    } else if url.path() == "/" {
        root_path.join("___Neovim__Unnamed_Buffer___.py")
    } else {
        url.to_file_path()
            .expect("Failed to convert URL to file path")
    }
}
