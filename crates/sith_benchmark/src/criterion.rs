pub use criterion::*;

pub type BenchmarkGroup<'a> = criterion::BenchmarkGroup<'a, measurement::WallTime>;
