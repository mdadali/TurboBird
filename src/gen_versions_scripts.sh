#!/bin/bash
# gen_versions_scripts.sh
set -e

# ==============================================================================
# [MODIFICA PER MACOS]: Creazione della funzione 'sedi' (sed in-place).
# Il comando 'uname' restituisce "Darwin" su macOS.
# Se siamo su Mac, aggiungiamo l'argomento vuoto '' richiesto da BSD sed.
# Se siamo su Linux (o altri), usiamo il normale -i di GNU sed.
# ==============================================================================
sedi() {
  if [ "$(uname)" = "Darwin" ]; then
    sed -i '' "$@"
  else
    sed -i "$@"
  fi
}
# ==============================================================================

LPI_FILE="TurboBird.lpi"
SRC_DIR="$(cd "$(dirname "$0")" && pwd)"       # Assumption: script is located in src/
HANDLE_SCRIPTS_DIR="$SRC_DIR/handle_scripts"
VERSION_INC="$SRC_DIR/version.inc"

mkdir -p "$HANDLE_SCRIPTS_DIR"

# 🔍 Helper function: extract value from LPI file
# (La lettura con sed standard o grep funziona allo stesso modo su entrambi gli OS, nessuna modifica qui)
get_value() {
  grep "$1" "$LPI_FILE" | sed -n 's/.*Value="\([^"]*\)".*/\1/p' | head -n1
}

# Determine project name
PROJECT_NAME=$(get_value "<Title")
if [ -z "$PROJECT_NAME" ]; then
  echo "❌ Project name not found in $LPI_FILE"
  exit 1
fi

echo "📛 Project name: $PROJECT_NAME"

# Get number of BuildModes from LPI
MODE_COUNT=$(grep '<BuildModes Count=' "$LPI_FILE" | sed -n 's/.*Count="\([0-9]*\)".*/\1/p')
echo "🔢 BuildModes found: $MODE_COUNT"

# Read version from version.inc
if [ ! -f "$VERSION_INC" ]; then
  echo "❌ version.inc not found: $VERSION_INC"
  exit 1
fi

read_version() {
  local file="$1"
  VERSION_MAJOR=$(grep VERSION_MAJOR "$file" | sed -n 's/.*= *\([0-9]*\);/\1/p')
  VERSION_MINOR=$(grep VERSION_MINOR "$file" | sed -n 's/.*= *\([0-9]*\);/\1/p')
  VERSION_REVISION=$(grep VERSION_REVISION "$file" | sed -n 's/.*= *\([0-9]*\);/\1/p')
  VERSION_BUILD=$(grep VERSION_BUILD "$file" | sed -n 's/.*= *\([0-9]*\);/\1/p')
  VERSION_STR=$(grep VERSION "$file" | grep -v VERSION_MAJOR | grep -v VERSION_MINOR | grep -v VERSION_REVISION | grep -v VERSION_BUILD | sed -n "s/.*= *'\([^']*\)'.*/\1/p")
}
read_version "$VERSION_INC"

FULL_VERSION="${VERSION_MAJOR}.${VERSION_MINOR}.${VERSION_REVISION}.${VERSION_BUILD}"
echo "ℹ️ Current version from version.inc: $FULL_VERSION"

# Generate/update handle scripts
for ((i=1; i<=MODE_COUNT; i++)); do
  MODE=$(grep "<Item${i}[ >]" "$LPI_FILE" | grep -o 'Name="[^"]*"' | head -n1 | cut -d'"' -f2)
  if [ -z "$MODE" ]; then
    echo "⚠️ BuildMode Item${i} not found, skipping..."
    continue
  fi

  SCRIPT_PATH="$HANDLE_SCRIPTS_DIR/handle_file_${MODE}.sh"

  if [ -f "$SCRIPT_PATH" ]; then
    echo "♻️ Updating handle script: $SCRIPT_PATH (Version & Project name)"
    # ==========================================================================
    # [MODIFICA PER MACOS]: Qui veniva usato "sed -i". 
    # Ora usiamo la nostra funzione "sedi" per evitare l'errore su macOS.
    # ==========================================================================
    sedi "s/^PROJECT_NAME=\"[^\"]*\"/PROJECT_NAME=\"${PROJECT_NAME}\"/" "$SCRIPT_PATH"
    sedi "s/^BUILD_MODE=\"[^\"]*\"/BUILD_MODE=\"${MODE}\"/" "$SCRIPT_PATH"
    sedi "s/^FULL_VERSION=\"[^\"]*\"/FULL_VERSION=\"${FULL_VERSION}\"/" "$SCRIPT_PATH"
    continue
  fi

echo "🛠️ Creating handle script: $SCRIPT_PATH"

{
  # Variabler Header
  cat <<EOF
#!/bin/bash
# Auto-generated handle script for BuildMode: $MODE
set -e

PROJECT_NAME="$PROJECT_NAME"
BUILD_MODE="$MODE"
FULL_VERSION="$FULL_VERSION"
EOF

# Append the rest from base_script.txt (if it exists)
    BASE_FILE="$SRC_DIR/base_script.txt"
    if [ -f "$BASE_FILE" ]; then
      cat "$BASE_FILE"
    else
      echo "⚠️ base_script.txt not found – the rest of the script is empty."
      exit 1  # Abort the build process here
    fi
  } > "$SCRIPT_PATH" 
  
  chmod +x "$SCRIPT_PATH"
  
done

# Only generate choose_target_script.sh if it does not exist
CHOOSE_SCRIPT="$SRC_DIR/choose_target_script.sh"
if [ ! -f "$CHOOSE_SCRIPT" ]; then
  echo "🛠️ Creating: $CHOOSE_SCRIPT"

  cat > "$CHOOSE_SCRIPT" <<'EOF'
#!/bin/bash
set -e

# ==============================================================================
# [MODIFICA PER MACOS]: Iniettiamo la funzione 'sedi' anche nello script 
# autogenerato 'choose_target_script.sh', così se in futuro decommenti 
# la logica di aggiornamento della versione, funzionerà sia su Mac che su Linux.
# ==============================================================================
sedi() {
  if [ "$(uname)" = "Darwin" ]; then
    sed -i '' "$@"
  else
    sed -i "$@"
  fi
}
# ==============================================================================

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

# ==============================================================================
# [MODIFICA PER MACOS]: Anche in queste righe commentate, "sed -i" è stato 
# sostituito con "sedi". Se decidi di rimuovere i commenti (#), il codice 
# non andrà in crash su macOS.
# ==============================================================================
#if (( new_build != current_build )); then
#  echo "🔄 Updating version.inc → $new_build"
#  sedi "s/^\(\s*VERSION_BUILD\s*=\s*\)[0-9]*;/\1${new_build};/" "$VERSION_INC"
#
#  new_version="${VERSION_MAJOR}.${VERSION_MINOR}.${VERSION_REVISION}.${new_build}"
#  sedi "s/^\(\s*VERSION\s*=\s*'\)[0-9.]*\(';\)/\1${new_version}\2/" "$VERSION_INC"
#fi

filename=$(basename "$TARGET_FILE")
build_mode="${filename#*_}"
build_mode="${build_mode%-v*}"

handle_script="$HANDLE_SCRIPTS_DIR/handle_file_${build_mode}.sh"
#If the path contains .exe, remove it.
handle_script="${handle_script/.exe.sh/.sh}"


if [ ! -f "$handle_script" ]; then
  echo "❌ Handle script not found: $handle_script"
  exit 1
fi

echo "🚀 Running: $handle_script"
"$handle_script"
echo "✅ Done."
EOF

  chmod +x "$CHOOSE_SCRIPT"
else
  echo "📝 choose_target_script.sh already exists – not overwritten."
fi

echo "✅ Scripts successfully generated."