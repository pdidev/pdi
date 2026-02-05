# SPDX-FileCopyrightText: 2022 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
#
# SPDX-License-Identifier: BSD-3-Clause

import json
import sys

def init_result_json(result_json, json_data):
    result_json["context"] = {}
    for property in ["date", "host_name", "num_cpus", "mhz_per_cpu", "cpu_scaling_enabled", "caches", "library_build_type"]:
        result_json["context"][property] = json_data["context"][property]
    result_json["benchmarks"] = []

def append_benchmarks(result_json, json_data):
    for benchmark in json_data["benchmarks"]:
        result_json["benchmarks"].append(benchmark)

if __name__ == "__main__":
    if len(sys.argv) < 3:
        print("Usage: " + sys.argv[0] + " <file_1> <file_2> ... <merged_json>")

    result_json = {}

    for file_name in sys.argv[1:-1]:
        with open(file_name, "r") as file:
            json_file = json.load(file)
            if file_name == sys.argv[1]:
                init_result_json(result_json, json_file)
            append_benchmarks(result_json, json_file)

    with open(sys.argv[-1], "w") as output_file:
        json.dump(result_json, output_file, indent=4)
    