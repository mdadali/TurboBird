#!/bin/bash
# Auto-generated handle script for BuildMode: x86_64-linux64-gtk2-debug
set -e

PROJECT_NAME="TurboBird"
BUILD_MODE="x86_64-linux64-gtk2-debug"
FULL_VERSION="1.2.1.1266"

STRIP=OFF
COMPRESS=ON
RENAME=ON
DELETE_OLD_FILES=ON
LOGGING=ON

IS_EXE=false

# --- Optional logging ---
if [ "$LOGGING" = "ON" ]; then
  SCRIPT_PATH="$(realpath "$0")"
  SCRIPT_NAME="$(basename "$SCRIPT_PATH")"
  SCRIPT_DIR="$(dirname "$SCRIPT_PATH")"
  LOG_DIR="$SCRIPT_DIR/logs"
  mkdir -p "$LOG_DIR"
  LOG_FILE="$LOG_DIR/${SCRIPT_NAME%.*}.log"

  echo "📄 Logging enabled → $LOG_FILE"
  echo "🕒 Started: $(date)" > "$LOG_FILE"
  exec > >(tee -a "$LOG_FILE") 2>&1
  set -euxo pipefail
fi

log() {
  echo "$@"
  if [ "$LOGGING" = "ON" ]; then
    echo "$@" >> "$LOG_FILE"
  fi
}

# --- Version info only when rename is ON ---
if [ "$RENAME" = "ON" ]; then
  log "🚀 Starting handle script for $PROJECT_NAME [$BUILD_MODE], version $FULL_VERSION"
else
  log "🚀 Starting handle script for $PROJECT_NAME [$BUILD_MODE]"
fi

SRC="$SCRIPT_DIR/../../bin/$BUILD_MODE/${PROJECT_NAME}_$BUILD_MODE/${PROJECT_NAME}_$BUILD_MODE"

# Automatically select the .exe file if it exists
if [ -f "${SRC}.exe" ]; then
  SRC="${SRC}.exe"
  IS_EXE=true
fi

log "📌 SRC initially points to: $SRC"

handle_file() {
  local src="$1"
  DST="$src"

  # --- Optional RENAME and EXE handling ---
  if [ "$RENAME" = "ON" ]; then
    if [[ "$src" == *.exe ]]; then
      log "🔍 Detected .exe input file"
      local new_src="${src%.exe}"
      mv "$src" "$new_src"
      log "📁 Renamed: $src → $new_src"
      src="$new_src"
      IS_EXE=true
    fi

    DST="${src}-v$FULL_VERSION"
    mv "$src" "$DST"
    log "✅ File renamed with version: $DST"
  fi

  # --- Optional binary stripping ---
  if [ "$STRIP" = "ON" ]; then
    if file "$DST" | grep -q -e "ELF" -e "PE32"; then
      strip "$DST"
      log "🔧 Binary stripped: $DST"
    else
      log "⚠️ Skipping strip: unsupported format"
    fi
  fi

  chmod +x "$DST"
  log "🔐 Marked as executable: $DST"

  # --- Restore .exe extension if originally present ---
  if [ "$RENAME" = "ON" ] && [ "$IS_EXE" = true ]; then
    local final_dst="${DST}.exe"
    mv "$DST" "$final_dst"
    DST="$final_dst"
    log "📦 Final EXE name restored: $DST"
  fi

  # --- Optional compression ---
  if [ "$COMPRESS" = "ON" ]; then
    ZIP_FILE="${DST}.zip"
    log "📦 Compressing file, please wait..."
    zip -j -q "$ZIP_FILE" "$DST"
    ZIP_SIZE=$(stat -c%s "$ZIP_FILE")
    ORIG_SIZE=$(stat -c%s "$DST")

    if (( ZIP_SIZE < ORIG_SIZE )); then
      log "📦 Compressed: $ZIP_FILE (from $((ORIG_SIZE / 1024 / 1024))MB → $((ZIP_SIZE / 1024 / 1024))MB)"
    else
      log "⚠️ ZIP larger than original – deleting"
      rm -f "$ZIP_FILE"
    fi
  fi

# --- Cleanup old versions (optional) ---
DST_DIR=$(dirname "$DST")
DST_FILENAME=$(basename "$DST")

if [ "$RENAME" = ON ]; then
  BASENAME_RAW="${DST_FILENAME%-v*}"
  VERSION_TAG="-v$FULL_VERSION"
  KEEP_BIN="${BASENAME_RAW}${VERSION_TAG}"

  if [ "$IS_EXE" = true ]; then
    KEEP_EXE="${KEEP_BIN}.exe"
    KEEP_ZIP="${KEEP_EXE}.zip"
  else
    KEEP_EXE=""
    KEEP_ZIP="${KEEP_BIN}.zip"
  fi
else
  BASENAME_RAW="$DST_FILENAME"
  KEEP_BIN="$DST_FILENAME"
  if [ "$IS_EXE" = true ]; then
    KEEP_ZIP="${DST_FILENAME}.zip"
  else
    KEEP_ZIP="${DST_FILENAME}.zip"
  fi
  KEEP_EXE=""
fi

if [ "$DELETE_OLD_FILES" = ON ]; then
  log "🧹 Cleaning up old versions of: $BASENAME_RAW"

  # ZIP-Dateien bereinigen (INI-Dateien ausnehmen)
  find "$DST_DIR" -type f -name "${BASENAME_RAW}-v*.zip" \
    ! -name "$KEEP_ZIP" ! -name "*.ini" \
    -exec bash -c 'echo "🗑️ Deleting old ZIP: $1"; rm -v "$1"' _ '{}' ';'

  # Binärdateien bereinigen (keine ZIPs und keine INIs)
  find "$DST_DIR" -type f -name "${BASENAME_RAW}-v*" \
    ! -name "$KEEP_BIN" ! -name "$KEEP_EXE" ! -name "$KEEP_ZIP" \
    ! -name "*.zip" ! -name "*.ini" \
    -exec bash -c 'echo "🗑️ Deleting old binary: $1"; rm -v "$1"' _ '{}' ';'
fi
}

handle_file "$SRC"
