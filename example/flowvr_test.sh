#!/bin/sh
#=============================================================================
# Copyright (C) 2019-2021 Commissariat a l'energie atomique et aux energies alternatives (CEA)
# Copyright (C) 2019-2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

if [ "$#" -ne 3 ]; then
	echo "Error, expected 3 arguments:\n    $0 <PDI_EXAMPLE_DIR> <PDI_example_C> <flowvr_visu>"
	exit
fi

PDI_EXAMPLE_DIR="$1"
PDI_example_C="$2"
flowvr_visu="$3"

# Relocation of flowvr-config.sh
WKDIR="$(mktemp -d)"
cp "$(dirname "$(which flowvr-config.sh)")"/flowvr-*.sh "${WKDIR}"
FLOWVR_PREFIX="$(dirname "$(dirname "$(which flowvr-config.sh)")" | sed -e 's/[\/&]/\\&/g')"
sed "s/\bpwd\b/echo '${FLOWVR_PREFIX}'/" -i "${WKDIR}/flowvr-relocate.sh"
( cd "${WKDIR}" && bash -x "./flowvr-relocate.sh" )
. "${WKDIR}/flowvr-config.sh"
rm -rf "${WKDIR}"

flowvrd > flowvrd.log &
DAEMON_PID=$!

# give daemon time to open
sleep 2

python3 "${PDI_EXAMPLE_DIR}/flowvr.py" "${PDI_EXAMPLE_DIR}" "${PDI_example_C}" "${flowvr_visu}"
flowvr -a flowvr > flowvr.log

# save test status
FLOWVR_STATUS=$?

# kill daemon
kill $DAEMON_PID

# give daemon time to close
sleep 2

# exit with test status
exit $FLOWVR_STATUS
