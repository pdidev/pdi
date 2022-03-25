# PDI benchmarks

To run PDI benchmarks:

1. Use `-DBUILD_BENCHMARKING=ON` flag when calling cmake.
2. In your build directory call `ctest -R benchmark`.
   Wait, it can take several minutes.

Directory where the result file will be saved can be changed
by adding`-DBENCHMARK_RESULT_PATH=<absolute_destination_path>` flag when
calling cmake.

To merge all results call:

```bash
py merge_json_files.py result_1.json result_2.json result_3.json output_new.json
```


To compare 2 benchmark results call:

```bash
py compare_results.py output_old.json output_new.json
```

Third argument (default to 0.1) can be added to set the percentage
difference of benchmark result that will be marked as better/worse.

For example for 0.1 percentage difference:
```
1251.28245 ns   1234.24794 ns
```
result will mark them as equal, but for:
```
90264.06765 ns  81578.23954 ns
```
result will mark the second one as a better result.

To convert json result file to csv use:

```bash
py convert_json_to_csv.py output_new.json benchmark_result.csv
```
