use std::{
    fs::{self, ReadDir},
    path::{Path, PathBuf},
    process::{Command, Stdio},
    sync::LazyLock,
};

use ruff_python_resolver::{
    host::Host, python_platform::PythonPlatform, python_version::PythonVersion,
};
use rustc_hash::FxHashSet;
use serde::{Deserialize, Serialize};

pub mod interpreter;
pub mod nodes;

/// A set of file names that indicate a Python project root directory.
pub static ROOT_FILES: LazyLock<FxHashSet<&str>> = LazyLock::new(|| {
    FxHashSet::from_iter([
        ".git",
        "requirements.txt",
        "Pipfile",
        "pyproject.toml",
        "setup.py",
    ])
});

#[derive(Deserialize, Serialize, Debug)]
pub struct PythonPathResult {
    pub paths: Vec<PathBuf>,
}

pub fn get_python_platform() -> anyhow::Result<PythonPlatform> {
    Ok(match std::env::consts::OS {
        "linux" => PythonPlatform::Linux,
        "macos" => PythonPlatform::Darwin,
        "windows" => PythonPlatform::Windows,
        platform => return Err(anyhow::anyhow!("Unsupported platform `{platform}`")),
    })
}

pub fn get_python_doc(
    interpreter_path: impl AsRef<Path>,
    search_term: &str,
) -> anyhow::Result<Option<String>> {
    let output = Command::new(interpreter_path.as_ref())
        .args([
            "-c",
            [
                "import pydoc",
                "pydoc.pager=pydoc.plainpager",
                &format!("pydoc.help('{search_term}')"),
            ]
            .join(";")
            .as_str(),
        ])
        .stdout(Stdio::piped())
        .output()
        .map_err(|e| anyhow::anyhow!("Failed to get python documentation! {e}"))?;

    let output_str = String::from_utf8_lossy(&output.stdout);
    if output_str.starts_with("No Python documentation found for") {
        Ok(None)
    } else {
        Ok(Some(
            // Skip the line with "Help on ..."
            output_str.lines().skip(1).collect::<Vec<&str>>().join("\n"),
        ))
    }
}

pub fn get_python_version(interpreter_path: impl AsRef<Path>) -> anyhow::Result<PythonVersion> {
    let output = Command::new(interpreter_path.as_ref())
        .arg("--version")
        .stdout(Stdio::piped())
        .output()
        .map_err(|_| anyhow::anyhow!("Failed to get python version!"))?;

    let output_str = String::from_utf8_lossy(&output.stdout);
    let output_segments = output_str.split(" ").collect::<Vec<&str>>();
    let version_output = output_segments
        .get(1)
        .ok_or_else(|| anyhow::anyhow!("Incorrect Python version output!"))?;

    parse_python_version(version_output)
}

pub(crate) fn parse_python_version(version_str: &str) -> anyhow::Result<PythonVersion> {
    let version_segments = version_str.split(".").collect::<Vec<_>>();
    let [major, minor, ..] = version_segments.as_slice() else {
        return Err(anyhow::anyhow!("Incorrect Python version format!"));
    };

    Ok(match [*major, *minor] {
        ["3", "7"] => PythonVersion::Py37,
        ["3", "8"] => PythonVersion::Py38,
        ["3", "9"] => PythonVersion::Py39,
        ["3", "10"] => PythonVersion::Py310,
        ["3", "11"] => PythonVersion::Py311,
        ["3", "12"] => PythonVersion::Py312,
        ["3", "13"] => PythonVersion::Py313,
        _ => return Err(anyhow::anyhow!("Unsupported Python version!")),
    })
}

pub fn get_python_search_paths(
    interpreter_path: impl AsRef<Path>,
) -> anyhow::Result<PythonPathResult> {
    let output = Command::new(interpreter_path.as_ref())
        .args([
            "-c",
            [
                "import json, sys",
                "orig_sys_path = [p for p in sys.path if p != '' and 'lib-dynload' not in p]",
                "json.dump(dict(paths=orig_sys_path), sys.stdout)",
            ]
            .join(";")
            .as_str(),
        ])
        .stdout(Stdio::piped())
        .output()
        .map_err(|_| anyhow::anyhow!("Failed to execute python command"))?;

    let json_str = String::from_utf8_lossy(&output.stdout);
    // TODO: don't use `expect` here
    let result: PythonPathResult =
        serde_json::from_str(&json_str).expect("failed to deserialize JSON");

    Ok(PythonPathResult {
        paths: result
            .paths
            .into_iter()
            .filter(|path| path.exists() && path.is_dir())
            .collect(),
    })
}

pub fn get_python_module_names_in_path(path: impl AsRef<Path>) -> Vec<(String, PathBuf)> {
    assert!(path.as_ref().is_dir());

    let mut names = Vec::new();
    let Ok(entries) = fs::read_dir(path) else {
        return Vec::new();
    };

    for entry in entries
        .filter_map(|e| e.ok())
        .filter(|dir_entry| dir_entry.file_name() != "__pycache__")
    {
        let Ok(file_type) = entry.file_type() else {
            continue;
        };

        if file_type.is_dir() {
            let sub_path = entry.path();
            let init_file = sub_path.join("__init__.py");

            if init_file.exists() {
                if let Some(dir_name) = sub_path.file_name().and_then(|name| name.to_str()) {
                    names.push((dir_name.to_string(), init_file));
                }
            }
        } else if file_type.is_file() {
            let path = entry.path();
            if let Some(file_name) = path.file_name().and_then(|name| name.to_str()) {
                if file_name.ends_with(".py") && file_name != "__init__.py" {
                    names.push((file_name.trim_end_matches(".py").to_string(), path));
                }
            }
        }
    }

    names
}

/// Checks if the symbol name correspond to the file path in the form `bar/__init__.py`
/// or `foo/bar.py`.
pub fn is_python_module(name: &str, path: &Path) -> bool {
    is_python_dir_module(name, path)
        || path.ends_with(format!("{name}.py"))
        || path.ends_with(format!("{name}.pyi"))
}

fn is_python_dir_module(name: &str, path: &Path) -> bool {
    path.ends_with(format!("{name}/__init__.py")) || path.ends_with(format!("{name}/__init__.pyi"))
}

/// Finds the root directory of a Python project by looking for specific marker files.
///
/// Starting from the given path, this function searches for files that typically indicate
/// a Python project root (see [`ROOT_FILES`]). If no marker file is found in the current directory, it continues
/// searching in parent directories until either:
/// - A directory containing a marker file is found
/// - The root of the filesystem is reached
///
/// # Returns
///
/// * If a marker file is found in any directory, returns that directory
/// * If no marker file is found after reaching the root, returns the original path
pub fn find_python_project_root(origin_path: impl AsRef<Path>) -> PathBuf {
    let mut path = origin_path.as_ref();
    loop {
        let Ok(files) = fs::read_dir(path) else {
            break;
        };

        let has_root_file = files.filter_map(|e| e.ok()).any(|e| {
            let path = e.path();
            let file_name = path.file_name().map(|f| f.to_string_lossy()).unwrap();
            ROOT_FILES.contains(file_name.as_ref())
        });

        if has_root_file {
            return path.to_path_buf();
        }

        match path.parent() {
            Some(parent_path) => path = parent_path,
            None => break,
        }
    }

    origin_path.as_ref().to_path_buf()
}

#[derive(Debug, Clone)]
pub struct PythonHost {
    pub version: PythonVersion,
    pub search_paths: Vec<PathBuf>,
    pub platform: PythonPlatform,
}

impl PythonHost {
    pub fn new(interpreter: impl AsRef<Path>) -> Self {
        Self {
            version: get_python_version(interpreter.as_ref()).unwrap_or(PythonVersion::None),
            search_paths: get_python_search_paths(interpreter.as_ref())
                .map(|result| result.paths)
                .unwrap_or_default(),
            platform: get_python_platform().unwrap(),
        }
    }

    pub fn invalid() -> Self {
        Self {
            version: PythonVersion::None,
            search_paths: Vec::new(),
            platform: PythonPlatform::Linux,
        }
    }
}

impl Host for PythonHost {
    fn python_search_paths(&self) -> Vec<PathBuf> {
        self.search_paths.clone()
    }

    fn python_version(&self) -> PythonVersion {
        self.version
    }

    fn python_platform(&self) -> PythonPlatform {
        self.platform
    }
}
