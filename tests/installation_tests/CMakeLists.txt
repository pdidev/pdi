#=============================================================================
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
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.

cmake_minimum_required(VERSION 3.22...4.2)
project(pdi_installation_tests NONE)

include(CTest)

# The download, build & install commands that \ref Source_installation tells a
# user to run.
# This is a standalone project, not a module of the superbuild: it builds and
# installs a released %PDI of its own, which has nothing to do with the sources
# being tested, so the `installation' job of the CI drives it on its own.
#
# `RELEASED_VERSION' is the single place naming the latest release, `bin/release'
# keeps it up to date; both the script below and the copy the documentation
# `\snippet's get the version substituted in from it, so the page and this test
# can never name different versions.
file(READ "../../RELEASED_VERSION" PDI_RELEASED_VERSION)
string(STRIP "${PDI_RELEASED_VERSION}" PDI_RELEASED_VERSION)

configure_file(download_release.sh.in download_release.sh @ONLY)

find_program(BASH_PROGRAM bash REQUIRED)
# the page tells the user to download with `curl', so the test needs it too
find_program(CURL_PROGRAM curl REQUIRED)

# The test really downloads, builds and installs the release, so it is slow and
# it needs the network.
add_test(NAME documented_installation
		COMMAND "${BASH_PROGRAM}" "${CMAKE_CURRENT_BINARY_DIR}/download_release.sh")
set_tests_properties(documented_installation PROPERTIES
		LABELS network
		TIMEOUT 3600)
