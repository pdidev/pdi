#!/bin/bash
#=============================================================================
# Copyright (C) 2022 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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


TEST_DIR="${TEST_DIR:-${PWD}}"


cd "$(dirname "$0")/" # build_scripts
cd "../.."  # PDI distrib dir
SRCDIR="$(pwd)"


cd "${TEST_DIR}"
cd "$(mktemp -d)"


MAKEFLAGS="${MAKEFLAGS:--j}"


if [ "xprovided" = "x${PDI_LIBS}" ]
then
	CMAKE_FLAGS="${CMAKE_FLAGS} -DUSE_DEFAULT=SYSTEM -DUSE_Zpp=SYSTEM"
else
	CMAKE_FLAGS="${CMAKE_FLAGS} -DUSE_DEFAULT=EMBEDDED"
fi

if [ -n "${PDI_PLUGIN_PATH}" ]
then
	CMAKE_FLAGS="${CMAKE_FLAGS} -DINSTALL_PDIPLUGINDIR=${PDI_PLUGIN_PATH}"
fi

if [ "xspack" = "x${PDI_SYSTEM}" ]
then
	#TODO: Workaround cmake FindOpenGL does not expect GLU to require -I
	export CPATH="${CPATH}:$(pkg-config --variable=includedir glu)" || true
fi

if [ "xspack" = "x${PDI_SYSTEM}" -a "xprovided" != "x${PDI_LIBS}" ]
then
	#TODO: Workaround Doxygen fails to find iconv
	CMAKE_FLAGS="${CMAKE_FLAGS} -DCMAKE_INCLUDE_DIRECTORIES_BEFORE=ON"
	#TODO: Workaround Doxygen finds system libmd before its own
	CMAKE_FLAGS="${CMAKE_FLAGS} -DICONV_IN_GLIBC=OFF"
	#TODO: Workaround FTI fails to include zlib https://github.com/leobago/fti/issues/407
	export CFLAGS="${CFLAGS} $(pkg-config --cflags zlib)"
	export LDFLAGS="${LDFLAGS} $(pkg-config --libs-only-L zlib)"
fi

if [ "xspack" = "x${PDI_SYSTEM}" -a "xlatest" != "x${PDI_DEPS}" ]
then
	# Workaround NetCDF < 4.6.2 does not support NC_HAS_PARALLEL4
	CMAKE_FLAGS="${CMAKE_FLAGS} -DBUILD_NETCDF_PARALLEL=OFF"
fi

if [ "xspack" = "x${PDI_SYSTEM}" -a "xlatest" = "x${PDI_DEPS}" ]
then
	# Workaround https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/433
	CTEST_FLAGS="${CTEST_FLAGS} -Etest_05_C"
fi

if [ "xubuntu-bionic" = "x${PDI_SYSTEM}" -a "xprovided" != "x${PDI_LIBS}" ]
then
	#TODO: https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/195
	CMAKE_FLAGS="${CMAKE_FLAGS} -DBUILD_DECL_SION_PLUGIN=OFF"
fi

if [ "xubuntu-bionic" = "x${PDI_SYSTEM}" -a "xprovided" = "x${PDI_LIBS}" ]
then
	# only sequential NetCDF is provided as a package in Ubuntu
	CMAKE_FLAGS="${CMAKE_FLAGS} -DBUILD_NETCDF_PARALLEL=OFF"
fi

if [ "xubuntu-bionic" = "x${PDI_SYSTEM}" -a "xprovided" = "x${PDI_LIBS}" -a "x${PDI_MPI}" != "openmpi" ]
then
	#TODO: mpich flowvr package is missing
	CMAKE_FLAGS="${CMAKE_FLAGS} -DBUILD_FLOWVR_PLUGIN=OFF"
fi



cmake -DDIST_PROFILE=Devel ${CMAKE_FLAGS} "${SRCDIR}"
make ${MAKEFLAGS}
ctest --output-on-failure --timeout 90 ${CTEST_FLAGS}
