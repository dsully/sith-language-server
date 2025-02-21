use std::{
    ffi::OsStr,
    io::Write,
    process::{Command, Output, Stdio},
};

use anyhow::Context;

/// Arguments that are not allowed to be passed to `ruff check`.
pub(crate) const UNSUPPORTED_CHECK_ARGS: &[&str] = &[
    "--force-exclude",
    "--no-cache",
    "--no-fix",
    "--quiet",
    "--diff",
    "--exit-non-zero-on-fix",
    "-e",
    "--exit-zero",
    "--fix",
    "--fix-only",
    "-h",
    "--help",
    "--no-force-exclude",
    "--show-files",
    "--show-fixes",
    "--show-settings",
    "--show-source",
    "--silent",
    "--statistics",
    "--verbose",
    "-w",
    "--watch",
    "--stdin-filename",
    "--output-format",
];

/// Arguments that are not allowed to be passed to `ruff format`.
pub(crate) const UNSUPPORTED_FORMAT_ARGS: &[&str] = &[
    "--force-exclude",
    "--quiet",
    "-h",
    "--help",
    "--no-force-exclude",
    "--silent",
    "--verbose",
    "--stdin-filename",
];

const CHECK_ARGS: &[&str] = &[
    "check",
    "--no-fix",
    "--force-exclude",
    "--quiet",
    "--output-format",
    "json",
];

fn execute_ruff_command<'a, I>(
    ruff_path: impl AsRef<OsStr>,
    args: I,
    stdin_input: impl AsRef<[u8]>,
) -> crate::Result<Output>
where
    I: IntoIterator<Item = &'a str>,
{
    let mut cmd = Command::new(ruff_path);
    cmd.args(args);

    let mut child = cmd
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .context("Failed to execute ruff binary")?;
    let mut stdin = child.stdin.take().context("Failed to take stdin")?;

    let input_bytes = stdin_input.as_ref();
    std::thread::scope(|s| {
        s.spawn(move || {
            stdin
                .write_all(input_bytes)
                .expect("Failed to write to stdin")
        });
    });
    let output = child
        .wait_with_output()
        .context("Failed to wait on child")?;

    Ok(output)
}

pub(crate) fn run_ruff_check<'a, I>(
    ruff_path: impl AsRef<OsStr>,
    filename: &'a str,
    content: &str,
    extra_args: I,
) -> crate::Result<Output>
where
    I: IntoIterator<Item = &'a str>,
{
    execute_ruff_command(
        ruff_path,
        CHECK_ARGS
            .iter()
            .copied()
            .chain(std::iter::once("--stdin-filename"))
            .chain(std::iter::once(filename))
            .chain(extra_args),
        content,
    )
}

pub(crate) fn run_ruff_format<'a, I>(
    ruff_path: impl AsRef<OsStr>,
    filename: &'a str,
    content: &str,
    extra_args: I,
) -> crate::Result<Output>
where
    I: Iterator<Item = &'a str>,
{
    execute_ruff_command(
        ruff_path,
        ["--force-exclude", "--quiet", "--stdin-filename"]
            .iter()
            .copied()
            .chain(std::iter::once(filename))
            .chain(extra_args),
        content,
    )
}

where
    I: IntoIterator<Item = &'a str>,
{
}
