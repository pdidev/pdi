# - Finds Libyaml
#
# === Variables ===
#
# This module will set the following variables in your project:
#   yaml_FOUND           TRUE if Findyaml found yaml
#
# === Usage ===
#
# To use this module, simply run find_package(yaml) from a CMakeLists.txt.

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

cmake_minimum_required(VERSION 3.5)

include(FindPackageHandleStandardArgs)

find_package(yaml ${yaml_FIND_VERSION} QUIET CONFIG)

if(NOT TARGET yaml)

find_package(PkgConfig)

pkg_check_modules(yaml QUIET yaml-0.1)

add_library(yaml UNKNOWN IMPORTED)
set_property(TARGET yaml PROPERTY INTERFACE_LINK_LIBRARIES "${yaml_LINK_LIBRARIES}")
if(yaml_INCLUDE_DIRS)
	set_property(TARGET yaml PROPERTY INTERFACE_INCLUDE_DIRECTORIES "${yaml_INCLUDE_DIRS}")
endif()
if(yaml_CFLAGS_OTHER)
	set_property(TARGET yaml PROPERTY INTERFACE_COMPILE_OPTIONS "${yaml_CFLAGS_OTHER}")
endif()

find_package_handle_standard_args(yaml DEFAULT_MSG yaml_LINK_LIBRARIES)

endif(NOT TARGET yaml)
