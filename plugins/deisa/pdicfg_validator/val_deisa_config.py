#=============================================================================
# Copyright (c) 2020-2024 Centre national de la recherche scientifique (CNRS)
# Copyright (c) 2020-2024 Commissariat a l'énergie atomique et aux énergies alternatives (CEA)
# Copyright (c) 2020-2024 Institut national de recherche en informatique et en automatique (Inria)
# Copyright (c) 2020-2024 Université Paris-Saclay
# Copyright (c) 2020-2024 Université de Versailles Saint-Quentin-en-Yvelines
#
# SPDX-License-Identifier: MIT
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
