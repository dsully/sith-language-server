pub mod criterion;

use std::fmt::{Display, Formatter};
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;
use walkdir::WalkDir;

use url::Url;

/// Relative size of a test case. Benchmarks can use it to configure the time for how long a benchmark should run to get stable results.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub enum TestCaseSpeed {
    /// A test case that is fast to run
    Fast,

    /// A normal test case
    Normal,

    /// A slow test case
    Slow,
}

#[derive(Debug, Clone)]
pub struct TestCase {
    file: TestFile,
    speed: TestCaseSpeed,
}

impl TestCase {
    pub fn fast(file: TestFile) -> Self {
        Self {
            file,
            speed: TestCaseSpeed::Fast,
        }
    }

    pub fn normal(file: TestFile) -> Self {
        Self {
            file,
            speed: TestCaseSpeed::Normal,
        }
    }

    pub fn slow(file: TestFile) -> Self {
        Self {
            file,
            speed: TestCaseSpeed::Slow,
        }
    }
}

impl TestCase {
    pub fn code(&self) -> &str {
        &self.file.code
    }

    pub fn name(&self) -> &str {
        &self.file.name
    }

    pub fn speed(&self) -> TestCaseSpeed {
        self.speed
    }

    pub fn path(&self) -> PathBuf {
        TARGET_DIR.join(self.name())
    }
}

#[derive(Debug, Clone)]
pub struct TestFile {
    name: String,
    code: String,
}

impl TestFile {
    pub fn code(&self) -> &str {
        &self.code
    }

    pub fn name(&self) -> &str {
        &self.name
    }
}

pub static TARGET_DIR: std::sync::LazyLock<PathBuf> = std::sync::LazyLock::new(|| {
    cargo_target_directory().unwrap_or_else(|| PathBuf::from("target"))
});

fn cargo_target_directory() -> Option<PathBuf> {
    #[derive(serde::Deserialize)]
    struct Metadata {
        target_directory: PathBuf,
    }

    std::env::var_os("CARGO_TARGET_DIR")
        .map(PathBuf::from)
        .or_else(|| {
            let output = Command::new(std::env::var_os("CARGO")?)
                .args(["metadata", "--format-version", "1"])
                .output()
                .ok()?;
            let metadata: Metadata = serde_json::from_slice(&output.stdout).ok()?;
            Some(metadata.target_directory)
        })
}

impl TestFile {
    pub fn new(name: String, code: String) -> Self {
        Self { name, code }
    }

    #[allow(clippy::print_stderr)]
    pub fn try_download(name: &str, url: &str) -> Result<TestFile, TestFileDownloadError> {
        let url = Url::parse(url)?;

        let cached_filename = TARGET_DIR.join(name);

        if let Ok(content) = std::fs::read_to_string(&cached_filename) {
            Ok(TestFile::new(name.to_string(), content))
        } else {
            // File not yet cached, download and cache it in the target directory
            let response = ureq::get(url.as_str()).call()?;

            let content = response.into_string()?;

            // SAFETY: There's always the `target` directory
            let parent = cached_filename.parent().unwrap();
            if let Err(error) = std::fs::create_dir_all(parent) {
                eprintln!("Failed to create the directory for the test case {name}: {error}");
            } else if let Err(error) = std::fs::write(cached_filename, &content) {
                eprintln!("Failed to cache test case file downloaded from {url}: {error}");
            }

            Ok(TestFile::new(name.to_string(), content))
        }
    }
}

#[derive(Debug)]
pub enum TestFileDownloadError {
    UrlParse(url::ParseError),
    Request(Box<ureq::Error>),
    Download(std::io::Error),
}

impl From<url::ParseError> for TestFileDownloadError {
    fn from(value: url::ParseError) -> Self {
        Self::UrlParse(value)
    }
}

impl From<ureq::Error> for TestFileDownloadError {
    fn from(value: ureq::Error) -> Self {
        Self::Request(Box::new(value))
    }
}

impl From<std::io::Error> for TestFileDownloadError {
    fn from(value: std::io::Error) -> Self {
        Self::Download(value)
    }
}

impl Display for TestFileDownloadError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TestFileDownloadError::UrlParse(inner) => {
                write!(f, "Failed to parse url: {inner}")
            }
            TestFileDownloadError::Request(inner) => {
                write!(f, "Failed to download file: {inner}")
            }
            TestFileDownloadError::Download(inner) => {
                write!(f, "Failed to download file: {inner}")
            }
        }
    }
}

impl std::error::Error for TestFileDownloadError {}

const HOME_ASSISTANT_REPO_URL: &str = "https://github.com/home-assistant/core.git";
const HOME_ASSISTANT_COMMIT: &str = "37328c78c142d430abd6a5dd2c07fe93a1142743";

#[derive(Debug)]
pub enum HomeAssistantRepoError {
    GitError(String),
    IoError(std::io::Error),
    FileNotFound(PathBuf),
    DownloadError(TestFileDownloadError),
}

impl From<std::io::Error> for HomeAssistantRepoError {
    fn from(error: std::io::Error) -> Self {
        Self::IoError(error)
    }
}

impl From<TestFileDownloadError> for HomeAssistantRepoError {
    fn from(error: TestFileDownloadError) -> Self {
        Self::DownloadError(error)
    }
}

impl std::fmt::Display for HomeAssistantRepoError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::GitError(msg) => write!(f, "Git error: {}", msg),
            Self::IoError(err) => write!(f, "IO error: {}", err),
            Self::FileNotFound(path) => write!(f, "File not found: {:?}", path),
            Self::DownloadError(err) => write!(f, "Download error: {}", err),
        }
    }
}

impl std::error::Error for HomeAssistantRepoError {}

/// Ensures Home Assistant repository is cloned and up to date
pub fn ensure_home_assistant_repo() -> Result<PathBuf, HomeAssistantRepoError> {
    let repo_path = TARGET_DIR.join("homeassistant");
    if !repo_path.is_dir() {
        std::fs::create_dir(&repo_path).expect("Failed to create directory!");
        clone_repository(&repo_path)?;
    }
    Ok(repo_path)
}

fn clone_repository(repo_path: &Path) -> Result<(), HomeAssistantRepoError> {
    let mut successs = Command::new("git")
        .current_dir(repo_path)
        .arg("init")
        .status()
        .map_err(|e| HomeAssistantRepoError::GitError(format!("Failed to init repository: {}", e)))?
        .success();
    if !successs {
        return Err(HomeAssistantRepoError::GitError(
            "Git 'init' command failed".to_string(),
        ));
    }
    successs = Command::new("git")
        .current_dir(repo_path)
        .args(["remote", "add", "origin", HOME_ASSISTANT_REPO_URL])
        .status()
        .map_err(|e| {
            HomeAssistantRepoError::GitError(format!("Failed to add repository origin: {}", e))
        })?
        .success();
    if !successs {
        return Err(HomeAssistantRepoError::GitError(
            "Git 'remote add' command failed".to_string(),
        ));
    }
    successs = Command::new("git")
        .current_dir(repo_path)
        .args(["fetch", "origin", HOME_ASSISTANT_COMMIT])
        .status()
        .map_err(|e| HomeAssistantRepoError::GitError(format!("Failed to fetch commit: {}", e)))?
        .success();
    if !successs {
        return Err(HomeAssistantRepoError::GitError(
            "Git 'fetch' command failed".to_string(),
        ));
    }
    successs = Command::new("git")
        .current_dir(repo_path)
        .args(["reset", "--hard", HOME_ASSISTANT_COMMIT])
        .status()
        .map_err(|e| HomeAssistantRepoError::GitError(format!("Failed to reset commit: {}", e)))?
        .success();

    if !successs {
        return Err(HomeAssistantRepoError::GitError(
            "Git 'reset' command failed".to_string(),
        ));
    }

    Ok(())
}

/// Find all Python files in the home assistant repository
pub fn find_all_python_files() -> Result<Vec<PathBuf>, HomeAssistantRepoError> {
    let repo_path = ensure_home_assistant_repo()?;
    let mut python_files = Vec::new();

    for entry in WalkDir::new(&repo_path)
        .into_iter()
        .filter_map(Result::ok)
        .filter(|e| !e.file_type().is_dir())
    {
        let path = entry.path();
        if let Some(ext) = path.extension() {
            if ext == "py" {
                python_files.push(path.to_path_buf());
            }
        }
    }

    Ok(python_files)
}
