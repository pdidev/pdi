# SPDX-FileCopyrightText: Copyright (c) 2024-2025 Kitware SAS
# SPDX-FileCopyrightText: Copyright (c) 2025 Commissariat a l'energie atomique et aux energies alternatives (CEA)
# SPDX-License-Identifier: Apache 2.0

import subprocess
import sys
import os
import filecmp
import json
import numpy as np

binary_folder = sys.argv[1]
source_folder = sys.argv[2]
test_name     = sys.argv[3] 

env = os.environ.copy()
env["CATALYST_DATA_DUMP_DIRECTORY"] = binary_folder + '/' + test_name
env["CATALYST_IMPLEMENTATION_NAME"] = 'stub' # need to get the conduit json file for comparison
# env["CATALYST_IMPLEMENTATION_PATHS"] = '/local/home/jm280892/local_9_12_paraview_catalyst/build_catalyst3/lib/catalyst/'

env["PDI_PLUGIN_PATH"] = binary_folder + '/..'

if test_name == 'true_uniform':
    result = subprocess.run([binary_folder + "/TestPDICatalystGhost", binary_folder + "/pdi.yml"], env=env)
elif test_name == 'true_structured':
    result = subprocess.run([binary_folder + "/TestPDICatalystGhost", binary_folder + "/pdi_structured.yml"], env=env)

if(result.returncode != 0):
    exit(result.returncode)

################################################################

class leaf_value_info(object):
    def __init__(self, leaf_node):
        self.dtype              = leaf_node['dtype']
        self.number_of_elements = leaf_node['number_of_elements']  
        self.offset             = leaf_node['offset']
        self.stride             = leaf_node['stride']
        self.elements_byte      = leaf_node['element_bytes']
        self.endianness         = leaf_node['endianness']

def expected_vtk_ghost_type():
    array_exact = np.zeros((32,16), dtype=np.uint8)
    array_exact[0,:]  = 1
    array_exact[31,:] = 1

    array_exact[:,0] = 1
    array_exact[:,1] = 1

    array_exact[:,14] = 1
    array_exact[:,15] = 1

    return array_exact

def check_ghost_type(file_bin_json, expected_solution):
    description_json = file_bin_json+"_json"

    with open(description_json) as ff:
        data_description = json.load(ff)

    ghost_obj_description = data_description["catalyst"]["channels"]["grid"]["data"]["fields"]["vtkGhostType"]

    ghost_values = ghost_obj_description["values"]

    # get information of data corresponding to the leaf node
    node_info = leaf_value_info(ghost_values)

    with open(file_bin_json,'rb') as ff_bin:
        ff_bin.seek(node_info.offset)
        chunk = ff_bin.read(node_info.elements_byte*node_info.number_of_elements)
        print("Position actuelle :", ff_bin.tell())
        array = np.frombuffer(chunk, dtype=np.uint8)
        array = array.reshape([32,16])

    return np.array_equal(array, expected_solution)

################################################################

file_bin_json = binary_folder + "/" + test_name + "/execute_invc0_params.conduit_bin.1.0"

expected_solution = expected_vtk_ghost_type()
check_ghost_type(file_bin_json, expected_solution)

if check_ghost_type(file_bin_json, expected_solution) != True:
    print(f'Differences detected in vtkGhostType')
    exit(1)
else:
    print(f'No differences detected in vtkGhostType')
