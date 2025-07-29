#!/bin/bash

# 🔢 Basisversion
BASE_VERSION="1.2.1"

# 📄 Datei mit Buildnummer
BUILD_FILE="build.txt"

# 🔁 Buildnummer erhöhen
if [ ! -f "$BUILD_FILE" ]; then
    echo "0" > "$BUILD_FILE"
fi

BUILD_NUM=$(cat "$BUILD_FILE")
BUILD_NUM=$((BUILD_NUM + 1))
echo "$BUILD_NUM" > "$BUILD_FILE"

# 🧩 Volle Versionsnummer
FULL_VERSION="${BASE_VERSION}.${BUILD_NUM}"

# 📝 Für Lazarus-Programm (Info-Dialog)
echo "const APP_VERSION = '${FULL_VERSION}';" > version.inc

# 💾 Für spätere Scripts (Rename etc.)
echo "$FULL_VERSION" > version.txt
echo "$FULL_VERSION" > .last_version

echo "✔ Version generiert: $FULL_VERSION"
