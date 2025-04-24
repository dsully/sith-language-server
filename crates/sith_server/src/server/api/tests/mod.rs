use std::path::Path;

mod lsp_client;
mod references;

fn strip_temp_dir(path: &Path) -> &Path {
    let temp_dir = std::env::temp_dir();
    path.strip_prefix(temp_dir)
        .expect("Failed to strip temp directory from path!")
}
