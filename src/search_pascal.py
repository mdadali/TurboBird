import os

# Liste der Dateiendungen, die durchsucht werden sollen
PASCAL_EXTENSIONS = [".pas", ".lfm", ".lrs", ".lpi", ".lpk", ".dpr"]

def search_in_pascal_files(root_dir: str, search_term: str):
    search_term_lower = search_term.lower()
    for dirpath, _, filenames in os.walk(root_dir):
        for filename in filenames:
            if any(filename.lower().endswith(ext) for ext in PASCAL_EXTENSIONS):
                filepath = os.path.join(dirpath, filename)
                try:
                    with open(filepath, "r", encoding="utf-8", errors="ignore") as f:
                        for lineno, line in enumerate(f, start=1):
                            if search_term_lower in line.lower():
                                print(f"{filepath}:{lineno}: {line.strip()}")
                except Exception as e:
                    print(f"⚠️ Konnte Datei nicht lesen: {filepath} ({e})")

if __name__ == "__main__":
    import sys
    if len(sys.argv) < 3:
        print("Verwendung: python search_pascal.py <Verzeichnis> <Suchwort>")
    else:
        search_in_pascal_files(sys.argv[1], sys.argv[2])
