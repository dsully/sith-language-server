use std::{
    fs::{self, ReadDir},
    path::{Path, PathBuf},
    process::{Command, Stdio},
};

use ruff_python_resolver::{python_platform::PythonPlatform, python_version::PythonVersion};
use serde::{Deserialize, Serialize};

pub mod interpreter;
pub mod nodes;

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

pub fn get_python_version(interpreter_path: impl AsRef<Path>) -> anyhow::Result<PythonVersion> {
    let output = Command::new(interpreter_path.as_ref())
        .arg("--version")
        .stdout(Stdio::piped())
        .output()
        .map_err(|_| anyhow::anyhow!("Failed to get python version!"))?;

    let output_str = String::from_utf8(output.stdout).expect("python output is not valid UTF-8");
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

    let json_str = String::from_utf8(output.stdout).expect("python output is not valid UTF-8");
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

fn is_hidden(entry: &fs::DirEntry) -> bool {
    entry
        .file_name()
        .to_str()
        .is_some_and(|s| s.starts_with("."))
}

fn dirs_to_skip(entry: &fs::DirEntry) -> bool {
    let path = entry.path();
    is_hidden(entry) || path.ends_with("__pycache__") || path.ends_with("node_modules")
}

fn is_python_file(entry: &fs::DirEntry) -> bool {
    let path = entry.path();
    path.is_file() && path.extension().is_some_and(|ext| ext == "py")
}

fn contains_python_files_in_dir(entries: &mut ReadDir) -> bool {
    entries.any(|e| e.is_ok_and(|e| is_python_file(&e)))
}

fn scan_dir_for_python_files(path: impl AsRef<Path>, paths: &mut Vec<PathBuf>) {
    let Ok(entries) = fs::read_dir(path) else {
        return;
    };

    for entry in entries.filter_map(|e| e.ok()) {
        if dirs_to_skip(&entry) {
            continue;
        }

        if is_python_file(&entry) {
            paths.push(entry.path())
        }

        if entry.file_type().is_ok_and(|e| e.is_dir()) {
            let pathbuf = entry.path();
            let Ok(mut sub_dir_entries) = fs::read_dir(&pathbuf) else {
                continue;
            };

            if contains_python_files_in_dir(&mut sub_dir_entries) {
                scan_dir_for_python_files(pathbuf, paths);
            }
        }
    }
}

pub fn python_files_in_path(root: impl AsRef<Path>) -> Vec<PathBuf> {
    let mut files = Vec::new();

    scan_dir_for_python_files(root, &mut files);

    files
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
