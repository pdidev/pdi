#=============================================================================
# Copyright (C) 2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
# Copyright (C) 2026 Institut national de recherche en informatique et en automatique (Inria)
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

# Python (and Dask) in situ analytics through the Damaris `pyscript` support.
# Building is always possible; running requires Damaris built with Python support
# (and Dask for 3dmesh_dask.yml and 3dmesh_dask_client.yml), see README.md.
foreach(PY_EXAMPLE_FILE 3dmesh_py.yml 3dmesh_py.py 3dmesh_dask.yml 3dmesh_dask.py
		3dmesh_dask_client.yml 3dmesh_dask_client.py)
	configure_file(${PY_EXAMPLE_FILE} ${PY_EXAMPLE_FILE} COPYONLY)
endforeach()
# Damaris writes its log files to ./log
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/log)

add_executable(3dmesh_py 3dmesh_py.c)
target_link_libraries(3dmesh_py PDI::PDI_C MPI::MPI_C)
