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

import importlib
import yaml
import sys
import re

# list of scalar types
scalar_types = ['char',
                'int',
                'int8',
                'int16',
                'int32',
                'int64',
                'float',
                'double',
                'character',
                'integer',
                'logical',
                'real']

# adds all references from string to data_refs list
def add_to_data_ref(string, data_refs_list):
    desc_name_re = re.compile(r'\$\{(\w+)\}')
    if not desc_name_re.search(str(string)):
        desc_name_re = re.compile(r'\$(\w+)')
    if desc_name_re.search(str(string)):
        for match in desc_name_re.findall(str(string)):
            data_refs_list.append(match)
        return True
    return False

# validate value of the size (numeric or reference)
def val_size_value(value, data_refs_list):
    if isinstance(value, int):
        pass
    elif isinstance(value, float):
        raise NameError('\033[31mSize cannot be a float: ' + value + "\033[0m")
    else:
        if not add_to_data_ref(value, data_refs_list):
            raise NameError("\033[31m`" + str(value) + "' is not valid value\033[0m")

# validate size property (scalar/reference or list of scalars/references)
def val_size(size, data_refs_list):
    if isinstance(size, list):
        for element in size:
            val_size_value(element, data_refs_list)
    else:
        val_size_value(size, data_refs_list)

# validate array (check required properties and values)
def val_array(value, data_refs_list):
    if 'size' not in value:
        raise NameError("\033[31mArray must have `size' property: " + str(value) + "\033[0m")
    if 'subtype' not in value:
        raise NameError("\033[31mArray must have `subtype' property: " + str(value) + "\033[0m")
    val_size(value['size'], data_refs_list)
    if 'subsize' in value:
        val_size(value['subsize'], data_refs_list)
    if 'start' in value:
        val_size(value['start'], data_refs_list)

# validate record member (must have disp and consist desc structure)
def val_member(member, data_refs_list):
    if 'disp' not in member:
        raise NameError("\033[31mMember must have `disp' property: " + str(member) + "\033[0m")
    val_size(member['disp'], data_refs_list)
    val_desc(member, data_refs_list)

# validate record (must have buffersize and members)
def val_record(value, data_refs_list):
    if 'buffersize' not in value:
        raise NameError("\033[31mRecord must have `buffersize' property: " + str(value) + "\033[0m")
    val_size(value['buffersize'], data_refs_list)
    if 'members' not in value:
        raise NameError("\033[31mRecord must have `members' property: " + str(value) + "\033[0m")
    for member in value['members'].values():
        val_member(member, data_refs_list)

# validate descriptor
def val_desc(value, data_refs_list):
    if value in scalar_types:
        pass
    elif value['type'] in scalar_types:
        pass
    elif value['type'] == 'array':
        val_array(value, data_refs_list)
    elif value['type'] == 'record':
        val_record(value, data_refs_list)
    else:
        raise NameError('\033[31mInvalid descriptor type in: ' + str(value) + "\033[0m")        

# validate PDI data node
def val_data(data_root, data_list, data_refs_list):
    if data_root:
        for key, value in data_root.items():
            val_desc(value, data_refs_list)
            data_list.append(key)

# validate PDI metadata node
def val_metadata(metadata_root, metadata_list, data_refs_list):
    if metadata_root:
        for key, value in metadata_root.items():
            val_desc(value, data_refs_list)
            metadata_list.append(key)

# check if all references to metadata are correct
def check_data_refs(data_list, metadata_list, data_refs):
    for ref in data_refs:
        if ref not in (data_list + metadata_list):
            raise NameError('\033[31mReference: `$' + ref + "' is not referencing any data nor metadata\033[0m")

# validate all plugins
def val_plugins(plugins_root, data_list, metadata_list, data_refs_list):
    if plugins_root:
        for key, plugin in plugins_root.items():
            try:
                getattr(importlib.import_module("pdicfg_validator.val_"+key+"_config"), "val_"+key)(plugin, data_list, metadata_list, data_refs_list)
            except NameError as e:
                raise e
            except ImportError:
                print("Cannot load " + key + " validation module.")
                print("Make sure that you have installed " + key + " plugin and $PYTHONPATH is set correctly.")
                print("Type 'echo $PYTHONPATH'. It should contain: '$PATH_TO_PDI_PYTHON'")
                print("If it's not there add it by: export PYTHONPATH=$PATH_TO_PDI_PYTHON/pdicfg_validator:$PYTHONPATH")
                print("Default PATH_TO_PDI_PYTHON is /usr/local/lib/python3/dist-packages")
                raise        

def run_test(config_file_name):
    # list of declared data
    data_list = []

    #list of declared metadata
    metadata_list = []

    # metadata that is used (referenced)
    data_refs_list = []

    with open(config_file_name, 'r') as config_file:
        root = yaml.load(config_file)
        pdi_root = root.get('pdi', root) # if file has pdi subtree use it

        if 'plugins' in pdi_root:
            val_plugins(pdi_root.get('plugins', False), data_list, metadata_list, data_refs_list)
        
        if 'data' in pdi_root:
            val_data(pdi_root.get('data', False), data_list, data_refs_list)
        
        if 'metadata' in pdi_root:
            val_metadata(pdi_root.get('metadata', False), metadata_list, data_refs_list)
        
        check_data_refs(data_list, metadata_list, data_refs_list)
        
        print("\033[1;32m" + sys.argv[1] + "\033[0;32m is a valid PDI configuration file\033[0m")