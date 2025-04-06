# SithLSP Benchmarks

```shell
# Run once on the "baseline".
cargo bench -p sith_benchmark -- --save-baseline=main

# Compare against the "baseline".
cargo bench -p sith_benchmark -- --baseline=main

# Run the sith benchmarks.
cargo bench -p sith_benchmark parser -- --baseline=main
```
