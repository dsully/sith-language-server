use std::num::NonZeroUsize;

fn main() -> anyhow::Result<()> {
    let max_cpu_count = NonZeroUsize::new(4).unwrap();

    let worker_threads = std::thread::available_parallelism()
        .unwrap_or(max_cpu_count)
        .max(max_cpu_count);
    let server = sith_server::Server::new(
        NonZeroUsize::try_from(worker_threads).expect("a non-zero worker thread count"),
    )?;

    server.run()
}
