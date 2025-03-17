#=============================================================================
# Copyright (C) 2025 Commissariat a l'energie atomique et aux energies alternatives (CEA)
#
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
# * Redistributions of source code must retain the above copyright notice,
#   this list of conditions and the following disclaimer.
# * Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
# * Neither the names of CEA, nor the names of the contributors may be used to
#   endorse or promote products derived from this software without specific
#   prior written  permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
#=============================================================================

# Temporarily rename the existing targets if they exist
if(TARGET PDI_C)
    set_target_properties(PDI_C PROPERTIES OUTPUT_NAME PDI_C_TEMP)
endif()

if(TARGET PDI::pdi)
    set_target_properties(PDI::pdi PROPERTIES OUTPUT_NAME PDI_pdi_TEMP)
endif()

if(TARGET PDI::PDI_C)
    set_target_properties(PDI::PDI_C PROPERTIES OUTPUT_NAME PDI_PDI_C_TEMP)
endif()

add_library(PDI_C INTERFACE)
add_library(PDI::pdi   ALIAS PDI_C)
add_library(PDI::PDI_C ALIAS PDI_C)

set_target_properties(PDI_C PROPERTIES
    INTERFACE_INCLUDE_DIRECTORIES "${CMAKE_CURRENT_LIST_DIR}/../include"
)

# Remove the temporarily renamed targets
if(TARGET PDI_C_TEMP)
    remove_library(PDI_C_TEMP)
endif()

if(TARGET PDI_pdi_TEMP)
    remove_library(PDI_pdi_TEMP)
endif()

if(TARGET PDI_PDI_C_TEMP)
    remove_library(PDI_PDI_C_TEMP)
endif()
