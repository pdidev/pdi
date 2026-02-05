#!/bin/echo 'please source this file, do not run it'

# SPDX-FileCopyrightText: 2019-2023 Commissariat a l'energie atomique et aux energies alternatives (CEA)
#
# SPDX-License-Identifier: BSD-3-Clause

if [ -n "$ZSH_VERSION" ]; then
	PDI_DIR_TMP="$(readlink -f "$(dirname "${(%):-%x}")")"
elif [ -n "$BASH_VERSION" ]; then
	PDI_DIR_TMP="$(readlink -f "$(dirname "${BASH_SOURCE[0]}")")"
else
	echo "This script only supports Bash and Zsh." >&2
	return
fi

. "${PDI_DIR_TMP}/env.sh"
