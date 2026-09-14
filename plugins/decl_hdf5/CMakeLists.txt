#=============================================================================
# Copyright (C) 2015-2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
# Copyright (C) 2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
# Copyright (C) 2026 Julien Bigot <julien@julien-bigot.fr>
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
# # SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
#=============================================================================

set(HDF5_DEPS hdf5::hdf5)
if("${HDF5_IS_PARALLEL}")
	list(APPEND HDF5_DEPS MPI::MPI_C MPI::MPI_CXX)
endif()

# The plugin
add_library(pdi_decl_hdf5_plugin MODULE
		attribute_op.cxx
		dataset_op.cxx
		decl_hdf5.cxx
		collision_policy.cxx
		file_op.cxx
		hdf5_wrapper.cxx
		selection.cxx
		subfiling.cxx)
target_link_libraries(pdi_decl_hdf5_plugin PUBLIC PDI::PDI_plugins ${HDF5_DEPS})
set_target_properties(pdi_decl_hdf5_plugin PROPERTIES CXX_VISIBILITY_PRESET hidden)
set_target_properties(pdi_decl_hdf5_plugin PROPERTIES LIBRARY_OUTPUT_DIRECTORY "${PDI_PLUGIN_OUTPUT_DIRECTORY}")

# installation
install(TARGETS pdi_decl_hdf5_plugin
		LIBRARY DESTINATION "${INSTALL_PDIPLUGINDIR}"
)

# Tests
if("${BUILD_TESTING}")
	add_subdirectory(tests/)
endif()

if("${BUILD_BENCHMARKING}")
	add_subdirectory(benchmarks)
endif()
