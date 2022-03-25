#*******************************************************************************
# Copyright (C) 2022 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
# * Redistributions of source code must retain the above copyright
#   notice, this list of conditions and the following disclaimer.
# * Redistributions in binary form must reproduce the above copyright
#   notice, this list of conditions and the following disclaimer in the
#   documentation and/or other materials provided with the distribution.
# * Neither the name of CEA nor the names of its contributors may be used to
#   endorse or promote products derived from this software without specific
#   prior written permission.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.
#*****************************************************************************/

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
    