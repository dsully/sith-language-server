use std::{fs, io, path::Path};

/// Reads the entire contents of a file into a string, just like [`fs::read_to_string`].
/// The only difference is that this function can read non-UTF8 files.
pub fn read_to_string(path: impl AsRef<Path>) -> io::Result<String> {
    Ok(String::from_utf8_lossy(&fs::read(path)?).to_string())
}

/// Checks if `path` looks like a directory path.
/// Ex)
///     - Unix: /foo/bar/
///     - Windows: C:\foo\bar\
pub fn is_path_dir(path: impl AsRef<Path>) -> bool {
    let path_str = path.as_ref().as_os_str();
    #[cfg(unix)]
    {
        path_str.to_string_lossy().ends_with('/')
    }
    #[cfg(windows)]
    {
        path_str.to_string_lossy().ends_with('\\')
    }
}
