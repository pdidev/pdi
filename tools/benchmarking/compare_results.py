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

def setting_warning(result_1_msg, result_2_msg):
    print("[WARNING] Following settings don't match:")
    print("    " + sys.argv[1] + ":\t" + result_1_msg)
    print("    " + sys.argv[2] + ":\t" + result_2_msg)

def check_setting(result_1_context, result_2_context, setting):
    if (result_1_context[setting] != result_2_context[setting]):
        setting_warning(setting + " = " + str(result_1_context[setting]),
                        setting + " = " + str(result_2_context[setting]))

def check_system(result_1_context, result_2_context):
    check_setting(result_1_context, result_2_context, "host_name")
    check_setting(result_1_context, result_2_context, "num_cpus")
    check_setting(result_1_context, result_2_context, "mhz_per_cpu")
    result_1_caches = result_1_context["caches"]
    result_2_caches = result_2_context["caches"]
    if (len(result_1_caches) != len(result_2_caches)):
        setting_warning(str(len(result_1_caches)) + " caches", str(len(result_2_caches)) + " caches")
    else:
        for cache_1 in result_1_caches:
            cache_found = False
            for cache_2 in result_2_caches:
                if (cache_1["type"] == cache_2["type"] and cache_1["level"] == cache_2["level"]):
                    if (cache_1["size"] != cache_2["size"]):
                        setting_warning("Level " + str(cache_1["level"]) + " " + cache_1["type"] + " cache size: " + str(cache_1["size"]),
                                        "Level " + str(cache_2["level"])+ " " + cache_2["type"] + " cache size: " + str(cache_2["size"]))
                    cache_found = True
                    continue
            if (cache_found == False):
                setting_warning("Level " + str(cache_1["level"]) + " " + cache_1["type"] + " cache exists",
                                "Level " + str(cache_1["level"]) + " " + cache_1["type"] + " cache doesn't exist")

def compare_test(result_1_test, result_2_test, test_significance):
    if (result_1_test["cpu_time"] * test_significance < result_2_test["cpu_time"]) :
        print("{:<50} \033[92m{:<30} \033[91m{:<30}\033[0m".format(result_1_test["name"],
                                             "{:.5f}".format(result_1_test["cpu_time"]) + " " + result_1_test["time_unit"],
                                             "{:.5f}".format(result_2_test["cpu_time"]) + " " + result_2_test["time_unit"]))
    elif (result_2_test["cpu_time"] * test_significance < result_1_test["cpu_time"]):
        print("{:<50} \033[91m{:<30} \033[92m{:<30}\033[0m".format(result_1_test["name"],
                                             "{:.5f}".format(result_1_test["cpu_time"]) + " " + result_1_test["time_unit"],
                                             "{:.5f}".format(result_2_test["cpu_time"]) + " " + result_2_test["time_unit"]))
    else:
        print("{:<50} {:<30} {:<30}".format(result_1_test["name"],
                                             "{:.5f}".format(result_1_test["cpu_time"]) + " " + result_1_test["time_unit"],
                                             "{:.5f}".format(result_2_test["cpu_time"]) + " " + result_2_test["time_unit"]))

if __name__ == "__main__":
    if len(sys.argv) < 3:
        print("Usage: " + sys.argv[0] + " <result_file_1> <result_file_2> <test_significance>")

    if len(sys.argv) == 4:
        test_significance = 1.0 + float(sys.argv[3])
    else:
        test_significance = 1.1

    with open(sys.argv[1], "r") as result_file_1:
        with open(sys.argv[2], "r") as result_file_2:
            result_1_root = json.load(result_file_1)
            result_2_root = json.load(result_file_2)
            check_system(result_1_root["context"], result_2_root["context"])
            print("\033[1m{:<50} {:<30} {:<30}\033[0m".format("Benchmark name", sys.argv[1], sys.argv[2]))
            for benchmark_1 in result_1_root["benchmarks"]:
                for benchmark_2 in result_2_root["benchmarks"]:
                    if (benchmark_1["name"] == benchmark_2["name"]):
                        compare_test(benchmark_1, benchmark_2, test_significance)
                        continue
