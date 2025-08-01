#!/bin/bash
# Auto-generated handle script for BuildMode: i386_Win32-win-debug
set -e

PROJECT_NAME="TurboBird"
BUILD_MODE="i386_Win32-win-debug"
FULL_VERSION="1.2.1.1262"

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SRC="$SCRIPT_DIR/../../bin/$BUILD_MODE/${PROJECT_NAME}_$BUILD_MODE/${PROJECT_NAME}_$BUILD_MODE"

STRIP=ON
COMPRESS=ON

handle_file() {
  local src="$1"

  for i in {1..5}; do
    if [ -f "$src" ]; then break; fi
    echo "⏳ Waiting for file: $src (attempt $i)..."
    sleep 0.5
  done

  if [ ! -f "$src" ]; then
    echo "❌ File not found: $src"
    exit 1
  fi

  DST="${src}-v$FULL_VERSION"
  mv "$src" "$DST"
  echo "✅ File renamed: $DST"

  if [ "$STRIP" = "ON" ]; then
    if file "$DST" | grep -q -e "ELF" -e "PE32"; then
      strip "$DST"
      echo "🔧 Binary stripped: $DST"
    else
      echo "⚠️ Skipping strip: unsupported format"
    fi
  fi

  chmod +x "$DST"
  echo "🔐 Made executable: $DST"

  if [ "$COMPRESS" = "ON" ]; then
    ORIG_SIZE=$(stat -c%s "$DST")
    gzip -kf "$DST"
    GZ_SIZE=$(stat -c%s "$DST.gz")

    if (( GZ_SIZE < ORIG_SIZE )); then
      echo "📦 Compressed: $DST.gz (from $((ORIG_SIZE / 1024 / 1024))MB → $((GZ_SIZE / 1024 / 1024))MB)"
    else
      echo "⚠️ gzip larger than original – deleted"
      rm -f "$DST.gz"
    fi
  fi

  # Clean up old versions except current
  DIRNAME=$(dirname "$DST")
  BASENAME=$(basename "$src")
  echo "🧹 Cleaning old versions (excluding v$FULL_VERSION)..."
  find "$DIRNAME" -type f -name "$BASENAME-v*" ! -name "*-v$FULL_VERSION" ! -name "*-v$FULL_VERSION.gz" -exec rm -v {} \;

  echo "✅ Done with version $FULL_VERSION."
}

handle_file "$SRC"
