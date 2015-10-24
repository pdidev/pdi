# - Finds Libyaml
#
# === Variables ===
#
# This module will set the following variables in your project:
#   LibYaml_FOUND           TRUE if FindLibYaml found LibYaml
#   LibYaml_COMPILE_FLAGS   Compilation flags for LibYaml programs
#   LibYaml_INCLUDE_PATH    Include path(s) for LibYaml header
#   LibYaml_LINK_FLAGS      Linking flags for LibYaml programs
#   LibYaml_LIBRARIES       All libraries to link LibYaml programs against
#
# === Usage ===
#
# To use this module, simply run find_package(LibYaml) from a CMakeLists.txt.
# If you are happy with the auto-detected configuration, then you're done.
# If not, set both LibYaml_<lang>_LIBRARIES and  LibYaml_<lang>_INCLUDE_PATH.
# You may also set any other variables listed above, but these two are
# required. This will circumvent autodetection entirely.

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

# Protect against multiple inclusion, which would fail when already imported targets are added once more.
if(NOT TARGET yaml)

# include this to handle the QUIETLY and REQUIRED arguments
include(FindPackageHandleStandardArgs)
include(GetPrerequisites)
find_package(PkgConfig)

pkg_check_modules(LibYaml QUIET yaml-0.1)
# message("<LibYaml_LIBRARIES>=${LibYaml_LIBRARIES}")
# message("<LibYaml_LIBRARY_DIRS>=${LibYaml_LIBRARY_DIRS}")
# message("<LibYaml_LDFLAGS>=${LibYaml_LDFLAGS}")
# message("<LibYaml_LDFLAGS_OTHER>=${LibYaml_LDFLAGS_OTHER}")
# message("<LibYaml_INCLUDE_DIRS>=${LibYaml_INCLUDE_DIRS}")
# message("<LibYaml_CFLAGS>=${LibYaml_CFLAGS}")
# message("<LibYaml_CFLAGS_OTHER>=${LibYaml_CFLAGS_OTHER}")

# Create imported target
add_library(yaml UNKNOWN IMPORTED)
list(GET LibYaml_LIBRARIES 0 _LibYaml_LIBRARY_NAME)
set(_LibYaml_LIBRARIES_OTHER "${LibYaml_LIBRARIES}")
list(REMOVE_AT _LibYaml_LIBRARIES_OTHER 0)
find_library(_LibYaml_LIBRARY "${_LibYaml_LIBRARY_NAME}" HINTS ${LibYaml_LIBRARY_DIRS})
set_target_properties(yaml PROPERTIES
  IMPORTED_LOCATION             "${_LibYaml_LIBRARY}"
  INTERFACE_COMPILE_OPTIONS     "${LibYaml_CFLAGS};${LibYaml_CFLAGS_OTHER}"
  INTERFACE_INCLUDE_DIRECTORIES "${LibYaml_INCLUDE_DIRS}"
  INTERFACE_LINK_LIBRARIES      "${_LibYaml_LIBRARIES_OTHER};${LibYaml_LDFLAGS};${LibYaml_LDFLAGS_OTHER}"
)

find_package_handle_standard_args(LibYaml DEFAULT_MSG _LibYaml_LIBRARY)

endif(NOT TARGET yaml) 
