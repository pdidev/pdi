#!/usr/bin/env bash

set -euo pipefail

# ----------- Config ------------------
IMAGE="ghcr.io/pdidev/ubuntu/rolling/openmpi/all:v3"
SCRIPT_NAME="run.sh"
SCRIPT_DIR="./.ci_tmp"
REPORT_FILE="tests.xml"
TMP_DIR="/tmp_dir_test"
BINARY_PATH="./bin/build_and_run_all_tests"
# ------------------------------------

mkdir -p "$SCRIPT_DIR"
rm -f "$SCRIPT_DIR/$SCRIPT_NAME" "$REPORT_FILE"

# Make sure the binary exists and is executable
if [[ ! -f "$BINARY_PATH" ]]; then
  echo "⚠️  Binary not found at $BINARY_PATH"
  exit 1
fi

echo "📝 Generating $SCRIPT_NAME in $SCRIPT_DIR..."
cat <<EOF > "$SCRIPT_DIR/$SCRIPT_NAME"
#!/bin/bash
set -xe

JOBID="\$(echo "manual-run" | md5sum | cut -b 1)"
if [[ "01234567" == *"\${JOBID}"* ]]; then
  export PDI_PLUGIN_PATH=/tmp/pdi_plugins
fi

export MAKEFLAGS='-j 4'
export CTEST_FLAGS="--output-junit /tmp/tests.xml"
export TEST_DIR="/tmp_dir_test"
export SRCDIR="/src"

chmod +x /tmp/build_and_run_all_tests
/tmp/build_and_run_all_tests
EOF

chmod +x "$SCRIPT_DIR/$SCRIPT_NAME"

# Step 1: Create a container and inject the script
echo "🐳 Creating and preparing container..."
CONTAINER_ID=$(podman create \
  --userns=keep-id \
  -v "$PWD":/src \
  --tmpfs "$TMP_DIR:exec" \
  "$IMAGE" \
  bash /tmp/run.sh)

echo "📦 Copying script and binary into container..."
podman cp "$SCRIPT_DIR/$SCRIPT_NAME" "$CONTAINER_ID:/tmp/run.sh"
podman cp "$BINARY_PATH" "$CONTAINER_ID:/tmp/build_and_run_all_tests"

# DO NOT run podman exec here — container isn't running yet

echo "🚀 Starting container and running script..."
podman start --attach "$CONTAINER_ID"

echo "📄 Attempting to extract test report from container..."
if podman cp "$CONTAINER_ID":/tmp/tests.xml "$REPORT_FILE"; then
  echo "✅ Test report saved as $REPORT_FILE"
else
  echo "⚠️ Test report not found in container."
fi

echo "🧹 Cleaning up container..."
podman rm "$CONTAINER_ID" >/dev/null 2>&1 || true
