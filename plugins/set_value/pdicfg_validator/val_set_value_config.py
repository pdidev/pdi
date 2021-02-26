#=============================================================================
# Copyright (C) 2020-2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

# validates set_value plugin node

def val_desc_value(list_of_desc_value, data_list, data_refs_list):
    for desc_value in list_of_desc_value:
        for desc in desc_value.keys():
            data_refs_list.append(desc)

# pdi_operation -> "set", "share", "expose", "event", "release"
def val_pdi_operation(list_of_pdi_operations, data_list, data_refs_list):
    for operation_map in list_of_pdi_operations:
        for key, value in operation_map.items():
            if key == "event":
                continue
            if key not in ["set", "share", "expose", "release"]:
                raise NameError("\033[31m(Set value) Invlaid pdi operation: " + key + ". Available operations: set, share, expose\033[0m")
            if isinstance(value, list):
                if key == "release":
                    for desc in value:
                      data_refs_list.append(desc)
                else:
                    val_desc_value(value, data_list, data_refs_list)
            else:
                raise NameError("\033[31m(Set value) Descripotrs names must be in a list\033[0m")
    
def val_on_init(on_init_node, data_list, data_refs_list):
    if on_init_node:
        if isinstance(on_init_node, list):
            val_pdi_operation(on_init_node, data_list, data_refs_list)
        else:
            raise NameError("\033[31m(Set value) on_init must be a list of operations\033[0m")

def val_on_data(on_data_node, data_list, data_refs_list):
    if on_data_node:
        for key, value in on_data_node.items():
            data_refs_list.append(key)
            if isinstance(value, list):
                val_pdi_operation(value, data_list, data_refs_list)
            else:
                raise NameError("\033[31m(Set value) Each on_data subnode must be a list of operations\033[0m")

def val_on_event(on_event_node, data_list, data_refs_list):
    if on_event_node:
        for key, value in on_event_node.items():
            if isinstance(value, list):
                val_pdi_operation(value, data_list, data_refs_list)
            else:
                raise NameError("\033[31m(Set value) Each on_event subnode must be a list of operations\033[0m")

def val_on_finalize(on_finalize_node, data_list, data_refs_list):
    if on_finalize_node:
        if isinstance(on_finalize_node, list):
            val_pdi_operation(on_finalize_node, data_list, data_refs_list)
        else:
            raise NameError("\033[31m(Set value) on_finalize must be a list of operations\033[0m")


def val_set_value(set_value_root, data_list, metadata_list, data_refs_list):
    val_on_init(set_value_root.get("on_init", False), data_list, data_refs_list)
    val_on_data(set_value_root.get("on_data", False), data_list, data_refs_list)
    val_on_event(set_value_root.get("on_event", False), data_list, data_refs_list)
    val_on_finalize(set_value_root.get("on_finalize", False), data_list, data_refs_list)
