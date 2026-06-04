#!/bin/bash

# ============================================
# TurboBird - Clean src directory
# ============================================

echo "🧹 Cleaning up src directory..."

# Delete all .bak files (backups)
find src -type f -name "*.bak" -delete
echo "  ✓ Deleted .bak files"

# Delete compiled binaries (.exe, .dll, .so)
find src -type f \( -name "*.exe" -o -name "*.dll" -o -name "*.so" \) -delete
echo "  ✓ Deleted .exe, .dll, .so files"

# Delete all .lrs files (can be regenerated on build)
find src -type f -name "*.lrs" -delete
echo "  ✓ Deleted .lrs files"

# Delete all .res files (compiled resources)
find src -type f -name "*.res" -delete
echo "  ✓ Deleted .res files"

# .zip files are kept (do nothing)

echo "✅ Cleanup completed"
