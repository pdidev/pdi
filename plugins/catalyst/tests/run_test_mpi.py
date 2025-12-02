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
result = subprocess.run([mpi_exec, "-np", "4", binary_folder + "/TestPDICatalyst", binary_folder + "/pdi.yml"], env=env)

if(result.returncode != 0):
    exit(result.returncode)

# get endiannes of the computer
endianness = sys.byteorder

if(endianness == 'little'):
    # Check the initialize json dump for each rank.
    reference_initialize_json = source_folder + "/references/initialize_reference.json"
    for rank in range(4):
        actual_initialize_json = binary_folder + f"initialize_params.conduit_bin.4.{rank}_json"
        with open(reference_initialize_json) as ref_file:
            with open(actual_initialize_json) as actual_file:
                ref_json = json.load(ref_file)
                actual_json = json.load(actual_file)
                if ref_json.items() != actual_json.items():
                    # Ignore the length of the script path which depends on platform.
                    actual_json["catalyst"]["scripts"]["script1"]["number_of_elements"] = ref_json["catalyst"]["scripts"]["script1"]["number_of_elements"]
                    if ref_json.items() != actual_json.items():
                        print(f'Differences detected in file "{actual_initialize_json}" compared to reference "{reference_initialize_json}')
                        exit(1)

    # Check the execute json dump for each rank.
    for rank in range(4):
        reference_execute_json = source_folder + f"/references/execute_reference_rank{rank}.json"
        for step in range(9):
            filepath = binary_folder + f"execute_invc{step}_params.conduit_bin.4.{rank}_json"
            if not filecmp.cmp(reference_execute_json, filepath):
                print(f'Differences detected in file "{filepath}" compared to reference "{reference_execute_json}')
                exit(1)

    # Check the finalize json dump for each rank.
    reference_finalize_json = source_folder + "/references/finalize_reference.json"
    for rank in range(4):
        actual_finalize_json = binary_folder + f"finalize_params.conduit_bin.4.{rank}_json"
        if not filecmp.cmp(reference_finalize_json, actual_finalize_json):
            print(f'Differences detected in file "{actual_finalize_json}" compared to reference "{reference_finalize_json}')
            exit(1)

else:
    print(f'The reference solution is based on little endian. So it is not possible to check with big endian.')
    print(f'The test is marked as failed anyway for the moment.')
    exit(1)
