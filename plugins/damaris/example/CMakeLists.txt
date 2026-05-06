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

# copy yaml file, to binary folder, for example_damaris_plugin.c
configure_file(example_damaris_plugin.yml example_damaris_plugin.yml COPYONLY)
add_executable (example_damaris_plugin example_damaris_plugin.c)
target_link_libraries (example_damaris_plugin PDI::PDI_C MPI::MPI_CXX)

# copy yaml file, to binary folder, for example_damaris_plugin_with_is_client.c
configure_file(example_damaris_plugin_with_is_client.yml example_damaris_plugin_with_is_client.yml COPYONLY)
add_executable (example_damaris_plugin_with_is_client example_damaris_plugin_with_is_client.c)
target_link_libraries (example_damaris_plugin_with_is_client PDI::PDI_C MPI::MPI_CXX)

# copy xml file for damaris, to binary folder, for example_damaris_only.c
configure_file(example_damaris_api.xml example_damaris_api.xml COPYONLY)
add_executable(example_damaris_only example_damaris_only.c)
target_link_libraries(example_damaris_only ${Damaris_DEPS})
