# SPDX-FileCopyrightText: 2022 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
#
# SPDX-License-Identifier: BSD-3-Clause

import csv
import json
import sys

if __name__ == "__main__":
    if len(sys.argv) < 3:
        print("Usage: " + sys.argv[0] + " <result_file.json> <result_file.csv>")

    with open(sys.argv[1], "r") as result_json:
        result_root = json.load(result_json)
        with open(sys.argv[2], mode='w') as result_csv:
            writer = csv.writer(result_csv, delimiter=',', quotechar='"', quoting=csv.QUOTE_MINIMAL)
            writer.writerow(["Benchmark name", "iterations", "real_time", "cpu_time", "time_unit"])
            for benchmark in result_root["benchmarks"]:
                writer.writerow([benchmark["name"],
                                 benchmark["iterations"],
                                 benchmark["real_time"],
                                 benchmark["cpu_time"],
                                 benchmark["time_unit"]])
