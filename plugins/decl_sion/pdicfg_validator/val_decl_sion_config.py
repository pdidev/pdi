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

from pdicfg_validator import add_to_data_ref 

def val_file_name(file_node, data_refs_list):
    if not file_node:
        raise NameError("\033[31m(Decl'SION) Input/Output node must contain file node.\033[0m")
    add_to_data_ref(file_node, data_refs_list)

def val_vars(io_node, data_refs_list):
    variable_node = io_node.get("variable", False)
    if variable_node:
        if not isinstance(variable_node, list):
            print("Adding " + variable_node)
            data_refs_list.append(variable_node)
        else:
            raise NameError("\033[31m(Decl'SION) `variable' node cannot be an array, use `vars'.\033[0m")
    else:
        vars_node = io_node.get("vars", False)
        if vars_node:
            if not isinstance(vars_node, list):
                raise NameError("\033[31m(Decl'SION) `vars' node must contain array of data or metadata names.\033[0m")
            else:
                for data_name in vars_node:
                    data_refs_list.append(data_name)
        else:
            raise NameError("\033[31m(Decl'SION) Input/Output node must contain `variable' or `vars' node.\033[0m")

def val_select(select_node, data_refs_list):
    if select_node:
        add_to_data_ref(select_node, data_refs_list)

def val_input_output(io_node, data_refs_list):
    val_file_name(io_node.get("file", False), data_refs_list)
    val_vars(io_node, data_refs_list)
    val_select(io_node.get("select", False), data_refs_list)

def val_comm(comm_node, data_refs_list):
    if comm_node:
        if comm_node not in ["$MPI_COMM_WORLD, $MPI_COMM_SELF"]:
            add_to_data_ref(comm_node, data_refs_list)

def val_inputs_outputs_array(io_list, data_refs_list):
    if io_list:
        if isinstance(io_list, list):
            for io_node in io_list:
                val_input_output(io_node, data_refs_list)
        else:
            raise NameError("\033[31m(Decl'SION) `inputs'/`outputs' node must be an array.\033[0m")

# validates decl_sion plugin node
def val_decl_sion(decl_sion_root, data_list, metadata_list, data_refs_list):
    val_comm(decl_sion_root.get("communicator", False), data_refs_list)
    val_inputs_outputs_array(decl_sion_root.get("inputs", False), data_refs_list)
    val_inputs_outputs_array(decl_sion_root.get("outputs", False), data_refs_list)