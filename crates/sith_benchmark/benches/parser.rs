use sith_benchmark::criterion::{
    criterion_group, criterion_main, measurement::WallTime, BenchmarkId, Criterion, Throughput,
};
use sith_benchmark::{TestCase, TestFile, TestFileDownloadError};
use sith_python_ast::visitor::{self, Visitor};
use sith_python_ast::Stmt;
use sith_python_parser::parse_module;

fn create_test_cases() -> Result<Vec<TestCase>, TestFileDownloadError> {
    Ok(vec![
        TestCase::fast(TestFile::try_download("numpy/globals.py", "https://raw.githubusercontent.com/numpy/numpy/89d64415e349ca75a25250f22b874aa16e5c0973/numpy/_globals.py")?),
        TestCase::fast(TestFile::try_download("unicode/pypinyin.py", "https://raw.githubusercontent.com/mozillazg/python-pinyin/9521e47d96e3583a5477f5e43a2e82d513f27a3f/pypinyin/standard.py")?),
        TestCase::normal(TestFile::try_download(
            "pydantic/types.py",
            "https://raw.githubusercontent.com/pydantic/pydantic/83b3c49e99ceb4599d9286a3d793cea44ac36d4b/pydantic/types.py",
        )?),
        TestCase::normal(TestFile::try_download("numpy/ctypeslib.py", "https://raw.githubusercontent.com/numpy/numpy/e42c9503a14d66adfd41356ef5640c6975c45218/numpy/ctypeslib.py")?),
        TestCase::slow(TestFile::try_download(
            "large/dataset.py",
            "https://raw.githubusercontent.com/DHI/mikeio/b7d26418f4db2909b0aa965253dbe83194d7bb5b/tests/test_dataset.py",
        )?),
    ])
}

struct CountVisitor {
    count: usize,
}

impl<'a> Visitor<'a> for CountVisitor {
    fn visit_stmt(&mut self, stmt: &'a Stmt) {
        visitor::walk_stmt(self, stmt);
        self.count += 1;
    }
}

fn benchmark_parser(criterion: &mut Criterion<WallTime>) {
    let test_cases = create_test_cases().unwrap();
    let mut group = criterion.benchmark_group("parser");

    for case in test_cases {
        group.throughput(Throughput::Bytes(case.code().len() as u64));
        group.bench_with_input(
            BenchmarkId::from_parameter(case.name()),
            &case,
            |b, case| {
                b.iter(|| {
                    let parsed = parse_module(case.code());
                    if !parsed.is_valid() {
                        panic!("Input should be valid code!");
                    }

                    let mut visitor = CountVisitor { count: 0 };
                    visitor.visit_body(parsed.suite());
                    visitor.count
                });
            },
        );
    }

    group.finish();
}

criterion_group!(parser, benchmark_parser);
criterion_main!(parser);
