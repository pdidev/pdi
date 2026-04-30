set -euo pipefail

IMAGE="ghcr.io/pdidev/run_clang_format:v4"

echo "Collecting files from git..."

FILES=$(git ls-files \
  | grep -v '^vendor/' \
  | grep -E '\.(c|h|cc|hh|cpp|hpp|cxx|hxx)$')

if [ -z "$FILES" ]; then
  echo "No files to check."
  exit 0
fi

echo "Running clang-format container..."

podman run --rm \
  --userns=keep-id \
  -v "$PWD":/src \
  -w /src \
  "$IMAGE" \
  "$@" \
  $FILES
