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

# Check for activated but not supported options
# Currently doesn't support any options, including Fortran and Python
set(_no-pdi_forbidden_options
    BUILD_BENCHMARKING
    BUILD_DECL_HDF5_PLUGIN
    BUILD_DECL_NETCDF_PLUGIN
    BUILD_DEISA_PLUGIN
    BUILD_DOCUMENTATION
    BUILD_HDF5_PARALLEL
    BUILD_FORTRAN
    BUILD_PYTHON
    BUILD_MPI_PLUGIN
    BUILD_NETCDF_PARALLEL
    BUILD_PYCALL_PLUGIN
    BUILD_SERIALIZE_PLUGIN
    BUILD_SET_VALUE_PLUGIN
    # BUILD_TESTING # Enabled by CTest (by "include(CTest)" in the case of the example)
    BUILD_TRACE_PLUGIN
    BUILD_USER_CODE_PLUGIN
    BUILD_JSON_PLUGIN
)

foreach(option IN LISTS _no-pdi_forbidden_options)
    if(DEFINED ${option} AND ${option}) # Check that the variable exists before evaluating it
        message(FATAL_ERROR
            "no-PDI configuration: The option ${option} must remain OFF when using no-PDI. "
        )
    endif()
endforeach()

unset(_no-pdi_forbidden_options)
# Enf of check

add_library(PDI_C INTERFACE)
add_library(PDI::pdi   ALIAS PDI_C)
add_library(PDI::PDI_C ALIAS PDI_C)

set_target_properties(PDI_C PROPERTIES
    INTERFACE_INCLUDE_DIRECTORIES "${CMAKE_CURRENT_LIST_DIR}/../include"
)

find_package(paraconf QUIET COMPONENTS C)

if("${paraconf_FOUND}")
    target_link_libraries(PDI_C INTERFACE paraconf::paraconf)
    target_compile_definitions(PDI_C INTERFACE PARACONF_FOUND)
endif()
