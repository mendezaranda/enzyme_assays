import os
from pathlib import Path

# === CONFIGURATION ===
# Path to the directory where your .csv files are stored.
# If you left the placeholder, this will default to the current directory.
workdir = r"C:\path\to\your\directory"  # <-- replace this with your real path, or set to "."

# Whether to also check inside subfolders (set to True if needed)
recursive = False


def add_clean_suffix(directory):
    """Rename files that end with .csv to have _clean before the extension.

    Examples:
      data.csv -> data_clean.csv
      some.name.csv -> some.name_clean.csv

    Files that already end with _clean.csv are skipped.
    """
    directory = Path(directory)
    if not directory.exists():
        print(f"⚠️ Directory does not exist: {directory}")
        return

    renamed = 0
    skipped = 0
    for root, _, files in os.walk(directory):
        for file in files:
            base, ext = os.path.splitext(file)
            if ext.lower() != ".csv":
                skipped += 1
                continue
            # skip files already marked as _clean
            if base.endswith("_clean"):
                skipped += 1
                continue

            old_path = Path(root) / file
            new_name = f"{base}_clean{ext}"
            new_path = Path(root) / new_name
            try:
                os.rename(old_path, new_path)
                print(f"Renamed: {file} → {new_name}")
                renamed += 1
            except Exception as e:
                print(f"❌ Failed to rename {file}: {e}")
        if not recursive:
            break

    print(f"\n✅ Done: renamed {renamed} files, skipped {skipped} files.")


if workdir == r"C:\path\to\your\directory":
    # user didn't change the default; use current directory
    workdir = Path('.')

add_clean_suffix(workdir)
