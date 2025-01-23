use std::{
    env,
    fs::{self, File},
    io,
    path::{Path, PathBuf},
};

use directories::BaseDirs;
use log::debug;

use crate::TYPESHED_ZIP_BYTES;

fn unzip_typeshed(path: &Path) {
    let mut typeshed_zip_archive =
        zip::ZipArchive::new(io::Cursor::new(TYPESHED_ZIP_BYTES)).unwrap();

    for i in 0..typeshed_zip_archive.len() {
        let mut file = typeshed_zip_archive.by_index(i).unwrap();
        let outpath = path.join(file.mangled_name());
        debug!("Unzipping {}", outpath.display());

        if file.is_dir() {
            std::fs::create_dir_all(&outpath).unwrap();
        } else {
            let mut outfile = File::create(&outpath).unwrap();
            io::copy(&mut file, &mut outfile).unwrap();
        }
    }
}

/// Unzips the typeshed folder at the data directory of the user executing sith-lsp.
///
/// The unzip process only happens when this function is called for the first time, subsequent
/// calls to this function will only return the path were the data was unzipped.
///
/// If this function fails to get the user data directory, the current directory is used instead.
/// The data directory in Linux is `$XDG_DATA_HOME` or `$HOME/.local/share`; in MacOS is `$HOME/Library/Application Support`;
/// in Windows is `{FOLDERID_RoamingAppData}`.
pub(crate) fn setup_typeshed() -> PathBuf {
    let sith_dir = if let Some(base_dirs) = BaseDirs::new() {
        base_dirs.data_dir().join("sith-lsp")
    } else {
        debug!("Failed to get user data directory, using current directory.");
        env::current_dir()
            .expect("permission to access current directory")
            .as_path()
            .join("sith-lsp")
    };

    if !sith_dir.is_dir() {
        fs::create_dir_all(&sith_dir).expect("permission to create directories");
    }

    let typeshed_path = sith_dir.join("typeshed");
    if !typeshed_path.is_dir() {
        fs::create_dir(&typeshed_path).expect("permission to create directory");
        unzip_typeshed(&typeshed_path);
    }

    typeshed_path
}
