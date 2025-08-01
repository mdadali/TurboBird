#!/bin/bash

CLEAN_OLD=true
COMPRESS=true  # ⬅️ Toggle compression: true = enable, false = disable
STRIP=true     # ⬅️ Toggle stripping: true = enable, false = disable

# 🔢 Read version from file
FULL_VERSION=$(cat version.txt)

# 📂 Search for output file
BASE_DIR="../bin"
MATCHING_FILE=$(find "$BASE_DIR" -type f -name "TurboBird_*" -path "*/TurboBird_*" | grep -v "\-v[0-9]" | head -n 1)

if [ -z "$MATCHING_FILE" ]; then
    echo "❌ No file to process found."
    exit 1
fi

# 🔍 Analyze path
SRC="$MATCHING_FILE"
DIRNAME=$(dirname "$SRC")
FILENAME=$(basename "$SRC")

# 🧩 Extract BUILD_MODE from filename (remove prefix "TurboBird_")
BUILD_MODE="${FILENAME#TurboBird_}"

# 🧩 New target with version
DST="${SRC}-v${FULL_VERSION}"

# ✅ Rename
mv "$SRC" "$DST"
echo "✅ Renamed: $DST"

# 🔐 Always make file executable
chmod +x "$DST"
echo "🔐 Made executable: $DST"

# 🔧 Strip binary if enabled
if [ "$STRIP" = true ]; then
    strip "$DST"
    echo "🔧 Stripped binary: $DST"
fi

# 📦 Compress if enabled
if [ "$COMPRESS" = true ]; then
    gzip -k "$DST"
    echo "📦 Compressed: ${DST}.gz (original kept)"
fi

# 🧹 Delete old versions if flag is set
if [ "$CLEAN_OLD" = true ]; then
    DIRNAME=$(dirname "$SRC")
    FILENAME=$(basename "$SRC")
    echo "🧹 Deleting old versions except v${FULL_VERSION}..."

    if [ "$COMPRESS" = true ]; then
        find "$DIRNAME" -type f -name "${FILENAME}-v*" ! -name "${FILENAME}-v${FULL_VERSION}" ! -name "${FILENAME}-v${FULL_VERSION}.gz" -exec rm -v {} \;
    else
        find "$DIRNAME" -type f -name "${FILENAME}-v*" ! -name "${FILENAME}-v${FULL_VERSION}" -exec rm -v {} \;
    fi
fi
