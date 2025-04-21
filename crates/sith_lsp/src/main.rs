use std::num::NonZeroUsize;

const VERSION: &str = env!("CARGO_PKG_VERSION");

fn main() -> anyhow::Result<()> {
    if let Some(arg) = std::env::args().nth(1) {
        match arg.as_str() {
            "--version" | "-v" => {
                println!("{}", VERSION);
            }
            "--help" | "-h" => {
                print_help();
            }
            _ => {
                println!("Unknow option '{arg}'. Use --help to list the available options.");
            }
        }
        Ok(())
    } else {
        run_server()
    }
}

fn run_server() -> anyhow::Result<()> {
    let max_cpu_count = NonZeroUsize::new(4).unwrap();

    let worker_threads = std::thread::available_parallelism()
        .unwrap_or(max_cpu_count)
        .max(max_cpu_count);
    let server = sith_server::Server::new(
        NonZeroUsize::try_from(worker_threads).expect("a non-zero worker thread count"),
    )?;

    server.run()
}

fn print_help() {
    println!(
        "SithLSP: An experimental An experimental Python language server made in Rust.

Usage: sith-lsp [OPTIONS]

Options:
  -v, --version    Print version
  -h, --help       Print help
"
    );
}
