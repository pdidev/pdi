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
