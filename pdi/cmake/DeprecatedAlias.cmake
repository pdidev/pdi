################################################################################
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
################################################################################

include_guard()

### Honour OLD, the deprecated name of the cache path NEW, as the default of NEW
#
# Only a top-level project does: when PDI is embedded, OLD belongs to the enclosing project.
# OLD is removed from the cache once read, so that a superbuild forwarding its cache does not hand it to a dependency that reads a variable of the
# same name for its own purpose, as Paraconf does with INSTALL_CMAKEDIR.
# \param OLD the deprecated name
# \param NEW the name that replaces it
# \param DOCSTRING the documentation of NEW, should OLD give it its value
###
function(pdi_deprecated_alias OLD NEW DOCSTRING)
	if(NOT "${PROJECT_IS_TOP_LEVEL}" OR NOT DEFINED "${OLD}")
		return()
	endif()
	message(DEPRECATION "${OLD} is deprecated, set ${NEW} instead.")
	if(NOT DEFINED "${NEW}")
		set("${NEW}" "${${OLD}}" CACHE PATH "${DOCSTRING}")
	elseif(NOT "${${OLD}}" STREQUAL "${${NEW}}")
		# The fallback only applies while NEW is absent from the cache, so changing OLD on an already configured build directory would otherwise be
		# ignored silently.
		message(WARNING
			"Both ${OLD} and ${NEW} are set and they disagree (${OLD}=${${OLD}}, ${NEW}=${${NEW}}); ${NEW} is the one that takes effect. "
			"Set ${NEW}, or configure a fresh build directory.")
	endif()
	unset("${OLD}" CACHE)
endfunction()
