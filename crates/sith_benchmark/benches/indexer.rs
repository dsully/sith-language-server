use sith_benchmark::criterion::{
    criterion_group, criterion_main, measurement::WallTime, BenchmarkId, Criterion, Throughput,
};
use sith_benchmark::{ensure_home_assistant_repo, find_all_python_files};
use sith_python_utils::interpreter::resolve_python_interpreter;
use sith_python_utils::PythonHost;
use sith_semantic_model::db::SymbolTableDb;

fn compute_throughput() -> u64 {
    let python_files =
        find_all_python_files().expect("failed to set up/get homeassistant directory");
    python_files
        .into_iter()
        .map(|file| {
            let metadata = file.metadata().expect("failed to get file metadata");
            metadata.len()
        })
        .sum()
}

fn benchmark_indexer(criterion: &mut Criterion<WallTime>) {
    let repo_path =
        ensure_home_assistant_repo().expect("Failed to initialize Home Assistant repository");

    let mut group = criterion.benchmark_group("indexer");

    let interpreter =
        resolve_python_interpreter(&repo_path).expect("Failed to find valid python interpreter!");
    let python_host = PythonHost::new(&interpreter);

    let file = repo_path.join("homeassistant/core.py");
    let content = std::fs::read_to_string(&file).expect("Failed to read file");

    group.throughput(Throughput::Bytes(compute_throughput()));
    group.bench_with_input(
        BenchmarkId::from_parameter("homeassistant"),
        &content,
        |b, _| {
            b.iter(|| {
                let _ = SymbolTableDb::new(repo_path.clone(), python_host.clone());
            });
        },
    );

    group.finish();
}

criterion_group!(indexer, benchmark_indexer);
criterion_main!(indexer);
