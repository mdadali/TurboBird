#!/bin/bash
set -e

VERSION_INC="$(dirname "$0")/version.inc"
HANDLE_SCRIPTS_DIR="$(dirname "$0")/handle_scripts"

if [ ! -f "$VERSION_INC" ]; then
  echo "❌ version.inc not found: $VERSION_INC"
  exit 1
fi

TARGET_FILE="$1"
if [ -z "$TARGET_FILE" ]; then
  echo "❌ No target file path provided!"
  echo "Usage: $0 /path/to/target_file"
  exit 1
fi

if [ ! -f "$TARGET_FILE" ]; then
  echo "❌ Target file not found: $TARGET_FILE"
  exit 1
fi

# 📖 Read version from version.inc
read_version() {
  local file="$1"
  VERSION_MAJOR=$(grep VERSION_MAJOR "$file" | sed -n 's/.*= *\([0-9]*\);/\1/p')
  VERSION_MINOR=$(grep VERSION_MINOR "$file" | sed -n 's/.*= *\([0-9]*\);/\1/p')
  VERSION_REVISION=$(grep VERSION_REVISION "$file" | sed -n 's/.*= *\([0-9]*\);/\1/p')
  VERSION_BUILD=$(grep VERSION_BUILD "$file" | sed -n 's/.*= *\([0-9]*\);/\1/p')
}

read_version "$VERSION_INC"

current_build=$VERSION_BUILD
if [ -z "$current_build" ]; then current_build=0; fi
echo "ℹ️ Build number from version.inc: $current_build"

max_build=0
all_equal=true
all_builds=()
for script in "$HANDLE_SCRIPTS_DIR"/handle_file_*.sh; do
  [ -f "$script" ] || continue
  ver=$(grep '^FULL_VERSION=' "$script" | sed 's/.*="\([0-9.]*\)".*/\1/')
  build=$(echo "$ver" | awk -F. '{print $4}')
  all_builds+=("$build")
  (( build > max_build )) && max_build=$build
done

# Check if all builds are equal
first="${all_builds[0]}"
for b in "${all_builds[@]}"; do
  if [[ "$b" != "$first" ]]; then
    all_equal=false
    break
  fi
done

new_build=$current_build
if (( max_build > current_build )); then
  echo "ℹ️ Higher build number detected: $max_build → using it"
  new_build=$max_build
elif $all_equal && (( current_build == max_build )); then
  (( new_build++ ))
  echo "🔼 All synchronized → increasing build number to $new_build"
else
  echo "ℹ️ No build number change"
fi

if (( new_build != current_build )); then
  echo "🔄 Updating version.inc → $new_build"
  sed -i "s/^\(\s*VERSION_BUILD\s*=\s*\)[0-9]*;/\1${new_build};/" "$VERSION_INC"

  new_version="${VERSION_MAJOR}.${VERSION_MINOR}.${VERSION_REVISION}.${new_build}"
  sed -i "s/^\(\s*VERSION\s*=\s*'\)[0-9.]*\(';\)/\1${new_version}\2/" "$VERSION_INC"
fi

filename=$(basename "$TARGET_FILE")
build_mode="${filename#*_}"
build_mode="${build_mode%-v*}"

handle_script="$HANDLE_SCRIPTS_DIR/handle_file_${build_mode}.sh"

if [ ! -f "$handle_script" ]; then
  echo "❌ Handle script not found: $handle_script"
  exit 1
fi

echo "🚀 Running: $handle_script"
"$handle_script"
echo "✅ Done."
