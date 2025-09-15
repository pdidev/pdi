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
env["PDI_PLUGIN_PATH"] = binary_folder + '/..'

result = subprocess.run([mpi_exec, "-np", "4", binary_folder + "/Test_tuto_september_2025", binary_folder + "/config.yml"], env=env)

if(result.returncode != 0):
    exit(result.returncode)
