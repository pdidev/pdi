#=============================================================================
# Copyright 2015 CEA, Julien Bigot <julien.bigot@cea.fr>
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

add_library(hdf5_per_process STATIC
	hdf5_per_process.c
)
set_target_properties(hdf5_per_process PROPERTIES C_VISIBILITY_PRESET hidden)
# target_link_libraries(hdf5_per_process pdi)
# set_property(TARGET pdi PROPERTY VERSION ${pdi_VERSION})
# set_property(TARGET pdi PROPERTY SOVERSION 3)
# set_property(TARGET pdi PROPERTY INTERFACE_pdi_MAJOR_VERSION 3)
# set_property(TARGET pdi APPEND PROPERTY COMPATIBLE_INTERFACE_STRING pdi_MAJOR_VERSION)



# library installation

install(TARGETS hdf5_per_process EXPORT hdf5_per_processTargets
	LIBRARY DESTINATION lib
	ARCHIVE DESTINATION lib
)

# cmake stuff installation

set(ConfigPackageLocation "lib/CMake/pdi-${pdi_VERSION}")
include(CMakePackageConfigHelpers)
install(EXPORT hdf5_per_processTargets DESTINATION ${ConfigPackageLocation})