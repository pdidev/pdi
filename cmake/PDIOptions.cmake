#=============================================================================
# Copyright (C) 2026 Julien Bigot <julien@julien-bigot.fr>
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
# * Redistributions of source code must retain the above copyright
#   notice, this list of conditions and the following disclaimer.
# * Redistributions in binary form must reproduce the above copyright
#   notice, this list of conditions and the following disclaimer in the
#   documentation and/or other materials provided with the distribution.
# * Neither the name of CEA nor the names of its contributors may be used to
#   endorse or promote products derived from this software without specific
#   prior written permission.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.
#=============================================================================

include_guard()

# The build settings a user of the distribution sets are PDI_-prefixed, so that the distribution can
# be embedded with add_subdirectory() without its settings colliding with variables of the same name
# in the enclosing project.
#
# When PDI is the top-level project the historical unprefixed name still works and provides the
# default, which keeps existing command lines, scripts and packaging recipes working unchanged.
# When PDI is embedded the unprefixed name is ignored: it belongs to the enclosing project and says
# nothing about how PDI should be built.
#
# The installation directories are the exception: they keep the unprefixed names that GNUInstallDirs
# uses for its own beside them, since everything installs to one prefix and a directory layout is not
# a per-project setting the way the build options are.

# Resolve the default of PDI_<NAME>, honouring an unprefixed <NAME> set by a top-level user.
# TRUTH says the setting is a boolean: ON/TRUE/1 and OFF/FALSE/0 then have to be compared as truth
# values rather than as text, or a preset saying `false' would look like it disagreed with `OFF'.
function(_pdi_resolve_default TRUTH NAME DEFAULT OUTVAR)
	if("${PROJECT_IS_TOP_LEVEL}" AND DEFINED "${NAME}")
		if(NOT DEFINED "PDI_${NAME}")
			set(DEFAULT "${${NAME}}")
		else()
			if("${TRUTH}")
				set(_PDI_UNPREFIXED FALSE)
				set(_PDI_PREFIXED FALSE)
				if("${${NAME}}")
					set(_PDI_UNPREFIXED TRUE)
				endif()
				if("${PDI_${NAME}}")
					set(_PDI_PREFIXED TRUE)
				endif()
			else()
				set(_PDI_UNPREFIXED "${${NAME}}")
				set(_PDI_PREFIXED "${PDI_${NAME}}")
			endif()
			if(NOT "${_PDI_UNPREFIXED}" STREQUAL "${_PDI_PREFIXED}")
				# The fallback only applies while PDI_<NAME> is absent from the cache, so changing
				# <NAME> on an already configured build directory would otherwise be ignored silently.
				message(WARNING
					"Both ${NAME} and PDI_${NAME} are set and they disagree "
					"(${NAME}=${${NAME}}, PDI_${NAME}=${PDI_${NAME}}); PDI_${NAME} is the one that "
					"takes effect. Set PDI_${NAME}, or configure a fresh build directory.")
			endif()
		endif()
	endif()
	set("${OUTVAR}" "${DEFAULT}" PARENT_SCOPE)
endfunction()

### Declare a boolean build setting, exposed to the user as PDI_<NAME>
function(pdi_option NAME DOCSTRING DEFAULT)
	_pdi_resolve_default(TRUE "${NAME}" "${DEFAULT}" _PDI_DEFAULT)
	option("PDI_${NAME}" "${DOCSTRING}" "${_PDI_DEFAULT}")
endfunction()

### Declare a string-valued build setting, exposed to the user as PDI_<NAME>
function(pdi_setting NAME DOCSTRING DEFAULT)
	_pdi_resolve_default(FALSE "${NAME}" "${DEFAULT}" _PDI_DEFAULT)
	set("PDI_${NAME}" "${_PDI_DEFAULT}" CACHE STRING "${DOCSTRING}")
endfunction()
