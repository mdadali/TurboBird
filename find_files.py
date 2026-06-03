#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Skript analysiert src-Verzeichnis: Zeigt Dateigrößen sortiert (alle Dateien)
"""

import os
import sys

def get_all_files_size(directory):
    """Sammelt alle Dateien mit Größen"""
    result = []
    
    for root, dirs, files in os.walk(directory):
        for file in files:
            full_path = os.path.join(root, file)
            try:
                size = os.path.getsize(full_path)
                result.append((full_path, size))
            except (OSError, PermissionError):
                pass
    return result

def main():
    directory = sys.argv[1] if len(sys.argv) > 1 else "./src"
    
    if not os.path.isdir(directory):
        print(f"Fehler: '{directory}' ist kein Verzeichnis.")
        sys.exit(1)
    
    print(f"Analysiere: {os.path.abspath(directory)}")
    print("-" * 60)
    
    files = get_all_files_size(directory)
    files.sort(key=lambda x: x[1], reverse=True)  # Größte zuerst
    
    total_size = sum(size for _, size in files)
    
    print(f"\n📁 Gesamtgröße: {total_size / (1024*1024):.2f} MB")
    print(f"📄 Anzahl Dateien: {len(files)}")
    print("\n🔍 Die 20 GRÖSSTEN Dateien:")
    print("-" * 60)
    
    for file_path, size in files[:20]:
        size_mb = size / (1024*1024)
        extension = os.path.splitext(file_path)[1] or "(keine)"
        print(f"{size_mb:>8.2f} MB | {extension:>10} | {file_path}")
    
    # Zusammenfassung nach Dateitypen
    print("\n📊 Zusammenfassung nach Dateiendungen:")
    print("-" * 60)
    extensions = {}
    for file_path, size in files:
        ext = os.path.splitext(file_path)[1] or "(keine Endung)"
        extensions[ext] = extensions.get(ext, 0) + size
    
    for ext, size in sorted(extensions.items(), key=lambda x: x[1], reverse=True)[:10]:
        size_mb = size / (1024*1024)
        print(f"{size_mb:>8.2f} MB | {ext}")

if __name__ == "__main__":
    main()
