#!/usr/bin/env bash
#=============================================================================
# Copyright (C) 2025-2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

# Use "./run-local-CI.sh" to run CI locally from a container

set -euo pipefail

# ----------- Config ------------------
IMAGE="ghcr.io/pdidev/ubuntu/rolling/openmpi/all:v4"
TMP_DIR="/tmp_dir_test"
BINARY_PATH="bin/build_and_run_all_tests"
# ------------------------------------

if command -v podman &>/dev/null; then
  CONTAINER_CMD="podman"
  USERNS_ARGS="--userns=keep-id"
elif command -v docker &>/dev/null; then
  CONTAINER_CMD="docker"
  USERNS_ARGS="--user $(id -u):$(id -g)"
else
  echo "Error: neither podman nor docker found in PATH." >&2
  exit 1
fi

REPO_ROOT=$(git rev-parse --show-toplevel)

if [[ ! -f "$REPO_ROOT/$BINARY_PATH" ]]; then
  echo "Script not found at $REPO_ROOT/$BINARY_PATH"
  exit 1
fi

echo -e "Running CI tests (using $CONTAINER_CMD)\n"

$CONTAINER_CMD run --rm \
  $USERNS_ARGS \
  -v "$REPO_ROOT":/src \
  --tmpfs "$TMP_DIR:exec" \
  -e MAKEFLAGS="-j 4" \
  -e TEST_DIR="/tmp_dir_test" \
  -e CMAKE_FLAGS="-DBUILD_DOCUMENTATION=OFF" \
  "$IMAGE" \
  /src/bin/build_and_run_all_tests

echo -e "\nDone"
