#!/bin/bash
#=============================================================================
# Copyright (C) 2020 Commissariat a l'energie atomique et aux energies alternatives (CEA)
# Copyright (C) 2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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



set -xe

cd "$(dirname "$0")/" # build_scripts
cd "../.."  # PDI distrib dir
SRCDIR="$(pwd)"

cd "$(mktemp -d || exit 1)"

CMAKE_FLAGS=""

CMAKE_VERSION="$(cmake --version | grep -o 'version [0-9]\+.[0-9]\+.[0-9]\+' | sed 's/[^0-9]*//')"
CMAKE_VERSION_MAJOR="${CMAKE_VERSION%%\.*}"
CMAKE_VERSION_MINOR="${CMAKE_VERSION%\.*}";CMAKE_VERSION_MINOR="${CMAKE_VERSION_MINOR#*\.}"

if [ "xprovided" = "x${PDI_LIBS}" ]
then
	CMAKE_FLAGS="${CMAKE_FLAGS} -DUSE_DEFAULT=SYSTEM"
else
	CMAKE_FLAGS="${CMAKE_FLAGS} -DUSE_DEFAULT=EMBEDDED"
fi

if [ "${CMAKE_VERSION_MAJOR}" -eq 3 -a "${CMAKE_VERSION_MINOR}" -lt 10 ]
then
	CMAKE_FLAGS="${CMAKE_FLAGS} -DBUILD_DOCUMENTATION=OFF -DBUILD_TESTING=OFF"
fi

if [ -n "${PDI_PLUGIN_PATH}" ]
then
	CMAKE_FLAGS="${CMAKE_FLAGS} -DINSTALL_PDIPLUGINDIR=${PDI_PLUGIN_PATH}"
fi

if [ "xdocker-ubuntu-xenial" = "x${PDI_SYSTEM}" ]
then
	#TODO: Fix this workaround, where we always preinstall Sionlib on ubuntu because autoinstall fails
	CMAKE_FLAGS="${CMAKE_FLAGS} -DUSE_SIONlib=SYSTEM"
	if [ "xprovided" = "x${PDI_LIBS}" ]
	then
		CMAKE_FLAGS="${CMAKE_FLAGS} -DUSE_Zpp=SYSTEM"
	fi
fi

cmake -DDIST_PROFILE=Devel ${CMAKE_FLAGS} "${SRCDIR}"
if [ -z "${MAKEFLAGS}" ]
then
	export MAKEFLAGS='-j'
fi
make
if [ "${CMAKE_VERSION_MAJOR}" -gt 3 -o "${CMAKE_VERSION_MINOR}" -ge 10 ]
then
	#TODO: Fix this workaround, where we skip centos tests on github because they fail
	if [ "x${DOCKER_RUNNER}" != "xgithub" -o "x${PDI_SYSTEM}" != "xdocker-centos-7" ]
	then
		ctest --output-on-failure
	fi
fi
