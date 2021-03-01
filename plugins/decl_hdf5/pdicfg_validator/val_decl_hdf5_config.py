#=============================================================================
# Copyright (C) 2018 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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
#=============================================================================

from pdicfg_validator import add_to_data_ref, val_desc

datasets = []

def val_file_name(file_node, data_refs_list):
    if not file_node:
        raise NameError("\033[31m(Decl'HDF5) File node must contain file name.\033[0m")
    add_to_data_ref(file_node, data_refs_list)
    
def val_comm(comm_node, data_refs_list):
    if comm_node:
        if comm_node not in ["$MPI_COMM_WORLD, $MPI_COMM_SELF"]:
            add_to_data_ref(comm_node, data_refs_list)

def val_datasets(datasets_node, data_refs_list):
    if datasets_node:
        for key, value in datasets_node.items():
            datasets.append(key)
            val_desc(value, data_refs_list)

def val_read_write_value(key, value_node, data_refs_list):
    #if dataset is not define, it's key value
    dataset_name = value_node.get("dataset", key)
    if dataset_name not in datasets:
        # means it is a reference to data
        data_refs_list.append(dataset_name)
    if value_node.get("when", False):
        add_to_data_ref(value_node["when"], data_refs_list)
    
    for node_name in ["memory_selection", "dataset_selection"]:
        if value_node.get(node_name, False):
            for value in value_node[node_name].values():
                if isinstance(value, list):
                    for element in value:
                        add_to_data_ref(element, data_refs_list)
                else:
                    add_to_data_ref(value, data_refs_list)
    
    if value_node.get("communicator", False):
        val_comm(value_node["communicator"], data_refs_list)
    if value_node.get("attribute", False):
        add_to_data_ref(value_node["attribute"], data_refs_list)
    if value_node.get("attributes", False):
        print(value_node["attributes"])
        for key, value in value_node["attributes"].items():
            add_to_data_ref(value, data_refs_list)

def val_read_write(read_write_node, data_refs_list):
    if isinstance(read_write_node, list):
        for data in read_write_node:
            data_refs_list.append(data)
    else:
        for key, value in read_write_node.items():
            val_read_write_value(key, value, data_refs_list)

def val_decl_hdf5_root(decl_hdf5_root, data_refs_list):
    val_file_name(decl_hdf5_root.get("file", False), data_refs_list)
    val_comm(decl_hdf5_root.get("communicator", False), data_refs_list)
    val_datasets(decl_hdf5_root.get("datasets", False), data_refs_list)
    if decl_hdf5_root.get("write", False):
        val_read_write(decl_hdf5_root["write"], data_refs_list)
    if decl_hdf5_root.get("read", False):
        val_read_write(decl_hdf5_root["read"], data_refs_list)

# validates decl_hdf5 plugin node
def val_decl_hdf5(decl_hdf5_root, data_list, metadata_list, data_refs_list):
    if isinstance(decl_hdf5_root, list):
        for file_config in decl_hdf5_root:
            val_decl_hdf5_root(file_config, data_refs_list)
    else:
        val_decl_hdf5_root(decl_hdf5_root, data_refs_list)
