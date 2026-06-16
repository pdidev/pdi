# SPDX-FileCopyrightText: Copyright (c) 2024-2025 Kitware SAS
# SPDX-FileCopyrightText: Copyright (c) 2025 Commissariat a l'energie atomique et aux energies alternatives (CEA)
# SPDX-License-Identifier: Apache 2.0

import subprocess
import sys
import os
import filecmp
import json

binary_folder = sys.argv[1]
source_folder = sys.argv[2]
mpi_exec = sys.argv[3]

env = os.environ.copy()
env["CATALYST_DATA_DUMP_DIRECTORY"] = binary_folder
env["CATALYST_IMPLEMENTATION_NAME"] = 'stub' # need to get the conduit json file for comparison
env["PDI_PLUGIN_PATH"] = binary_folder + '/..'

# execution 
result = subprocess.run([mpi_exec, "-np", "4", binary_folder + "/TestPDICatalystComm", binary_folder + "/pdi.yml"], env=env)

if(result.returncode != 0):
    exit(result.returncode)

# check file exit for the rank selected