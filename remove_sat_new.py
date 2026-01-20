import pandas as pd
import os
import re
import sys
from pathlib import Path

# === CONFIGURATION ===
workdir = Path(".")  # current directory
excel_file = workdir / "Raw_files.xlsx"
output_dir = workdir / "clean_data"
output_dir.mkdir(exist_ok=True)

# Supported cutoffs and prefixes
CUTOFFS = ["30", "60", "90", "120"]
PREFIXES = ["", "Full", "Malt", "Suc", "Lac", "Phlo", "Iso", "IMu", "FullSet", "MaltSet", "SucSet"]


def find_row_column(df):
    """Try to find the dataframe column that contains the row letter (A-H).
    Return column name or None.
    """
    candidates = [c for c in df.columns]
    # first try common names
    for name in ["Batch", "batch", "Row", "row", "Well", "well"]:
        for c in candidates:
            if c == name:
                return c

    # fallback: find a column whose values look like single letters (A-H)
    for c in candidates:
        try:
            vals = df[c].dropna().astype(str).unique()
        except Exception:
            continue
        # check if majority are single letters A-H
        letter_count = sum(1 for v in vals if re.fullmatch(r"[A-Ha-h]", v.strip()))
        if letter_count >= max(1, len(vals) // 2):
            return c

    return None


def find_column_for_number(df, col_number):
    """Try various strategies to match a numeric column label for well column.
    Returns the matching column name or None.
    """
    col_number = str(int(col_number))  # normalize like '01' -> '1'
    cols = [str(c) for c in df.columns]

    # exact match
    if col_number in cols:
        return col_number

    # match zero-padded
    for c in cols:
        if re.fullmatch(rf"0*{re.escape(col_number)}", c.strip()):
            return c

    # match columns that end with the number, e.g. 'A_1' or 'Col_1'
    for c in cols:
        if re.search(rf"\b0*{re.escape(col_number)}\b", c):
            return c

    return None


def parse_wells(cell):
    """Return a list of normalized wells ('A1') from a cell value.
    Accepts comma/semicolon separated strings, lists, numbers, or NaN.
    """
    if pd.isna(cell):
        return []
    # ensure a plain string (remove surrounding quotes if present)
    if isinstance(cell, (list, tuple)):
        items = cell
    else:
        s = str(cell).strip()
        # remove wrapping quotes
        if (s.startswith('"') and s.endswith('"')) or (s.startswith("'") and s.endswith("'")):
            s = s[1:-1]
        # handle special keyword 'All' (case-insensitive)
        if s.strip().lower() == 'all':
            # default plate size: 8 rows (A-H) x 12 cols (1-12)
            return [r + str(c) for r in list('ABCDEFGH') for c in range(1, 13)]
        items = re.split(r"[,;]", s)
    wells = []
    for it in items:
        it = str(it).strip().strip(',')
        if not it:
            continue
        m = re.match(r"^([A-Za-z])0*([0-9]+)$", it)
        if m:
            wells.append(m.group(1).upper() + str(int(m.group(2))))
        else:
            # keep as-is uppercase (e.g. if someone wrote 'G2 ')
            wells.append(it.upper())
    return wells


txt_file = workdir / "Raw_files.txt"
if txt_file.exists():
    try:
        raw_df = pd.read_csv(txt_file, sep='\t', header=0, dtype=str)
    except Exception as e:
        print(f" Failed to read Raw_files.txt: {e}")
        sys.exit(1)
else:
    if not os.path.exists(excel_file):
        print(f" Excel file not found at {excel_file} and no Raw_files.txt present. Put Raw_files.txt or Raw_files.xlsx in {workdir}")
        sys.exit(1)
    # Try reading the excel; header row can vary. First try header=2, else header=0
    try:
        raw_df = pd.read_excel(excel_file, header=2, dtype=str)
        # if the columns aren't as expected, fallback
        if "Plate" not in raw_df.columns:
            raw_df = pd.read_excel(excel_file, header=0, dtype=str)
    except Exception as e:
        print(f" Failed to read Excel file: {e}")
        sys.exit(1)

# Normalize columns: ensure columns like '30','60','90','120' exist
cols_lower = [str(c).lower() for c in raw_df.columns]
needed = {"30": None, "60": None, "90": None, "120": None}
for name in raw_df.columns:
    key = str(name).strip()
    if key in needed:
        needed[key] = key

# Make Plate column str for matching
if "Plate" not in raw_df.columns:
    # try to find a plausible plate column
    for c in raw_df.columns:
        if re.search(r"plate", str(c), re.IGNORECASE):
            raw_df = raw_df.rename(columns={c: "Plate"})
            break

raw_df["Plate"] = raw_df["Plate"].astype(str).str.strip()

# Extract valid plate-cutoff combinations from Excel
valid_plates = []
for _, row in raw_df.iterrows():
    plate = row["Plate"].strip()
    for cutoff in CUTOFFS:
        cell = row.get(cutoff)
        if pd.isna(cell):
            continue
        if str(cell).strip() == "":
            continue
        valid_plates.append((plate, cutoff))

processed = 0
skipped = 0

for file in os.listdir(workdir):
    # skip the excel file itself and any already-cleaned CSVs
    if not file.lower().endswith('.csv'):
        continue
    if file == os.path.basename(excel_file):
        continue
    if file.lower().endswith('_clean.csv'):
        continue

    # match cutoff by filename suffix (_30.csv, _60.csv, _90.csv, _120.csv)
    m = re.search(r"_(30|60|90|120)\.csv$", file, re.IGNORECASE)
    if not m:
        skipped += 1
        continue

    cutoff = m.group(1)
    plate_id = file[:m.start()]
    cutoff_col = cutoff

    # Check if this file matches any valid plate-cutoff combination from Excel
    found_match = False
    for valid_plate, valid_cutoff in valid_plates:
        if valid_plate == plate_id and valid_cutoff == cutoff:
            found_match = True
            break
    
    if not found_match:
        print(f" No matching plate-cutoff combination in Excel for '{file}', skipping")
        skipped += 1
        continue

    # Get the row from Excel
    row = raw_df.loc[raw_df["Plate"].str.strip() == plate_id]
    if row.empty:
        print(f" No entry for plate '{plate_id}' in Excel, skipping {file}")
        skipped += 1
        continue

    saturated_cell = row.iloc[0].get(cutoff_col)
    wells = parse_wells(saturated_cell)
    if not wells:
        print(f" No saturated wells for plate '{plate_id}' at cutoff {cutoff}, skipping {file}.")
        skipped += 1
        continue

    print(f"Processing {file}: {len(wells)} wells to clean: {wells}")

    csv_path = os.path.join(workdir, file)
    try:
        df = pd.read_csv(csv_path, dtype=object)
    except Exception as e:
        print(f" Failed to read {file}: {e}")
        skipped += 1
        continue

    # make column names strings for easier matching
    df.columns = [str(c) for c in df.columns]

    row_col = find_row_column(df)
    if not row_col:
        print(f" Could not find row-label column in {file}. Common names: 'Batch' or 'Row'. Skipping.")
        skipped += 1
        continue

    # Look up numeric columns for the wells to clean
    col_indices_found = []
    col_indices_missing = []
    for well in wells:
        if not well:
            continue
        m2 = re.match(r"^([A-Z])([0-9]+)$", well)
        if not m2:
            print(f" Unrecognized well format '{well}' in {file}; expected like 'A1'. Skipping that entry.")
            continue
        row_letter = m2.group(1)
        col_number = m2.group(2)
        col_name = find_column_for_number(df, col_number)
        if col_name:
            col_indices_found.append((well, row_letter, col_name))
        else:
            print(f" Could not find column for well {well} (looking for column {col_number})")
            col_indices_missing.append(well)

    if not col_indices_found:
        print(f" No columns found for any wells in {file}, skipping")
        skipped += 1
        continue

    # set the values to None in the target wells
    changed_any = False
    for well, row_letter, col_name in col_indices_found:
        row_mask = df[row_col].str.strip().str.upper() == row_letter
        if not any(row_mask):
            print(f" Could not find any rows with letter '{row_letter}' in column '{row_col}'")
            continue
        df.loc[row_mask, col_name] = None
        changed_any = True

    if not changed_any:
        print(f" No changes made to {file}, skipping output")
        skipped += 1
        continue

    output_file = output_dir / file.replace(".csv", "_clean.csv")
    df.to_csv(output_file, index=False)
    processed += 1
    print(f" Saved cleaned data to {output_file}")


print(f"\n Done! Processed {processed} files, skipped {skipped} files.")
