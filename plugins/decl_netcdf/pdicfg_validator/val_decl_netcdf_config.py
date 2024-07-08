#=============================================================================
# Copyright (C) 2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

variables = []

def val_when(when_node, data_refs_list):
    if when_node:
        add_to_data_ref(when_node, data_refs_list)

def val_file_name(file_node, data_refs_list):
    if not file_node:
        raise NameError("\033[31m(Decl'netcdf) File node must contain file name.\033[0m")
    add_to_data_ref(file_node, data_refs_list)

def val_comm(comm_node, data_refs_list):
    if comm_node:
        if comm_node not in ["$MPI_COMM_WORLD, $MPI_COMM_SELF"]:
            add_to_data_ref(comm_node, data_refs_list)

def val_variables(variables_node, data_refs_list):
    if variables_node:
        for key, value in variables_node.items():
            variables.append(key)
            val_desc(value, data_refs_list)
            attributes_node = value.get("attributes", False)
            if attributes_node:
                for key, value in attributes_node.items():
                    add_to_data_ref(value, data_refs_list)

def val_groups(groups_node, data_refs_list):
    if groups_node:
        for key, value in groups_node.items():
            attributes_node = value.get("attributes", False)
            if attributes_node:
                for key, value in attributes_node.items():
                    add_to_data_ref(value, data_refs_list)

def val_read_write_value(key, value_node, data_refs_list):
    #if variable is not defined, it's key value
    variable_name = value_node.get("variable", key)
    if variable_name not in variables:
        # means it is a reference to data
        data_refs_list.append(variable_name)
    val_when(value_node.get("when", False), data_refs_list)
    val_when(value_node.get("size_of", False), data_refs_list)
    
    var_selection = value_node.get("variable_selection", False)
    if var_selection:
        for value in var_selection.values():
            if isinstance(value, list):
                for element in value:
                    add_to_data_ref(element, data_refs_list)
            else:
                add_to_data_ref(value, data_refs_list)

def val_read_write(read_write_node, data_refs_list):
    if isinstance(read_write_node, list):
        for data in read_write_node:
            data_refs_list.append(data)
    else:
        for key, value in read_write_node.items():
            val_read_write_value(key, value, data_refs_list)

def val_decl_netcdf_root(decl_netcdf_root, data_refs_list):
    val_file_name(decl_netcdf_root.get("file", False), data_refs_list)
    val_when(decl_netcdf_root.get("when", False), data_refs_list)
    val_comm(decl_netcdf_root.get("communicator", False), data_refs_list)
    val_variables(decl_netcdf_root.get("variables", False), data_refs_list)
    val_groups(decl_netcdf_root.get("groups", False), data_refs_list)
    if decl_netcdf_root.get("write", False):
        val_read_write(decl_netcdf_root["write"], data_refs_list)
    if decl_netcdf_root.get("read", False):
        val_read_write(decl_netcdf_root["read"], data_refs_list)

# validates decl_netcdf plugin node
def val_decl_netcdf(decl_netcdf_root, data_list, metadata_list, data_refs_list):
    if isinstance(decl_netcdf_root, list):
        for file_config in decl_netcdf_root:
            val_decl_netcdf_root(file_config, data_refs_list)
    else:
        val_decl_netcdf_root(decl_netcdf_root, data_refs_list)
