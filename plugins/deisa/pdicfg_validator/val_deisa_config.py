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


def val_on_init(on_init_node, data_list, data_refs_list):
    if on_init_node:
        if not isinstance(on_init_node, str):
            raise NameError("\033[31m(Deisa) on_init must be a string \033[0m")


def val_on_scheduler_info(on_scheduler_info_node, data_list, data_refs_list):
    if on_scheduler_info_node:
        if not isinstance(on_scheduler_info_node, str):
            raise NameError("\033[31m(Deisa) scheduler_info must be a string \033[0m")


def val_deisa(deisa_root, data_list, metadata_list, data_refs_list):
    val_on_init(deisa_root.get("on_init", False), data_list, data_refs_list)
    val_on_scheduler_info(deisa_root.get("scheduler_info", False), data_list, data_refs_list)
