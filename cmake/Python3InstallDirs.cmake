################################################################################
# Copyright (C) 2015-2026 Commissariat a l'energie atomique et aux energies
# alternatives (CEA)
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

# This offers INSTALL_PYTHON3_SITEARCHDIR, the directory compiled Python modules install to, relative to the installation prefix.
# It is the site directory the Python3 interpreter reports for an empty prefix; a value already in the cache is kept and the interpreter
# is not queried.
#
# The variable is cached, so it is defined by the first directory that includes this module, and shared by every other.

include_guard(GLOBAL)

# Whoever found Python3 already did it with the version and components they need.
if(NOT TARGET Python3::Interpreter)
	find_package(Python3 REQUIRED COMPONENTS Interpreter)
endif()

if(NOT DEFINED CACHE{INSTALL_PYTHON3_SITEARCHDIR})
	# The target is visible wherever it was found from, unlike Python3_EXECUTABLE.
	get_target_property(_PYTHON3_INSTALL_DIRS_INTERPRETER Python3::Interpreter IMPORTED_LOCATION)
	execute_process(COMMAND "${_PYTHON3_INSTALL_DIRS_INTERPRETER}" -c [=[
import sys
try:
	import setuptools
except ModuleNotFoundError:
	pass
from distutils import sysconfig
sys.stdout.write(sysconfig.get_python_lib(prefix='', plat_specific=True, standard_lib=False))
]=]
		RESULT_VARIABLE _PYTHON3_INSTALL_DIRS_RESULT
		OUTPUT_VARIABLE _PYTHON3_INSTALL_DIRS_SITEARCHDIR
		ERROR_VARIABLE _PYTHON3_INSTALL_DIRS_ERROR
		OUTPUT_STRIP_TRAILING_WHITESPACE
		ERROR_STRIP_TRAILING_WHITESPACE)
	if(NOT "0" STREQUAL "${_PYTHON3_INSTALL_DIRS_RESULT}")
		message(FATAL_ERROR
			"Unable to query the Python module directory from `${_PYTHON3_INSTALL_DIRS_INTERPRETER}':\n"
			"${_PYTHON3_INSTALL_DIRS_ERROR}\n"
			"     * choose it explicitly => pass `-DINSTALL_PYTHON3_SITEARCHDIR=<dir>' to cmake"
		)
	endif()
	set(INSTALL_PYTHON3_SITEARCHDIR "${_PYTHON3_INSTALL_DIRS_SITEARCHDIR}"
		CACHE PATH "Python modules (${_PYTHON3_INSTALL_DIRS_SITEARCHDIR})")
	unset(_PYTHON3_INSTALL_DIRS_ERROR)
	unset(_PYTHON3_INSTALL_DIRS_INTERPRETER)
	unset(_PYTHON3_INSTALL_DIRS_RESULT)
	unset(_PYTHON3_INSTALL_DIRS_SITEARCHDIR)
endif()
