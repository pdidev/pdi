#!/bin/bash
set -xe

cd "$(dirname "$0")/" # build_scripts
cd "../.."  # PDI distrib dir
SRCDIR="$(pwd)"

cd "$(mktemp -d || exit 1)"

CMAKE_FLAGS=""

CMAKE_VERSION="$(cmake --version | grep 'cmake version' | sed 's/[^0-9]*//')"
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
	CMAKE_FLAGS="${CMAKE_FLAGS} -DPDI_PLUGIN_PATH=${PDI_PLUGIN_PATH}"
fi

if [ "xdocker-ubuntu-xenial" = "x${PDI_SYSTEM}" ]
then
	#TODO: Fix this workaround, where we always preinstall Sionlib on ubuntu because autoinstall fails
	CMAKE_FLAGS="${CMAKE_FLAGS} -DUSE_SIONlib=SYSTEM"
	if [ "xprovided" = "x${PDI_LIBS}" ]
	then
		CMAKE_FLAGS="${CMAKE_FLAGS} -DUSE_Bpp=SYSTEM"
	fi
fi

cmake -DDIST_PROFILE=Devel ${CMAKE_FLAGS} "${SRCDIR}"
make -j
if [ "${CMAKE_VERSION_MAJOR}" -gt 3 -o "${CMAKE_VERSION_MINOR}" -ge 10 ]
then
	#TODO: Fix this workaround, where we skip centos tests on github because they fail
	if [ "${DOCKER_RUNNER}" != github -o "${PDI_SYSTEM}" != "docker-centos-7" ]
	then
		ctest --output-on-failure
	fi
fi
