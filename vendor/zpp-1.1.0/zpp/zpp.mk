##############################################################################
# SPDX-FileCopyrightText: 2014-2024 Centre national de la recherche scientifique (CNRS)
# SPDX-FileCopyrightText: 2014-2024 Commissariat a l'énergie atomique et aux énergies alternatives (CEA)
# SPDX-FileCopyrightText: 2014-2024 Julien Bigot <julien.bigot@cea.fr>
# SPDX-FileCopyrightText: 2014-2024 Université Paris-Saclay
# SPDX-FileCopyrightText: 2014-2024 Université de Versailles Saint-Quentin-en-Yvelines
#
# SPDX-License-Identifier: MIT
##############################################################################

ZPP_COMPILER_ID:=Gnu
ZPP_PATH:=$(abspath $(lastword $(MAKEFILE_LIST))/../..)

ZPP=$(ZPP_PATH)/bin/zpp

%: %.zpp
	$(ZPP) -DZPP_CONFIG=config.$(ZPP_COMPILER_ID) $(ZPPOPTS) $(ZPPFLAGS) $< $@

%.F90: %.F90.zpp
	$(ZPP) -DZPP_CONFIG=config.$(ZPP_COMPILER_ID) $(ZPPOPTS) $(ZPPFLAGS) $< $@

%.h: %.h.zpp
	$(ZPP) -DZPP_CONFIG=config.$(ZPP_COMPILER_ID) $(ZPPOPTS) $(ZPPFLAGS) $< $@

%.inc: %.inc.zpp
	$(ZPP) -DZPP_CONFIG=config.$(ZPP_COMPILER_ID) $(ZPPOPTS) $(ZPPFLAGS) $< $@
