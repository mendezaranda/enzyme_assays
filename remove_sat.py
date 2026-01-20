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
    """Return a list of normalized wells (e.g. 'A1') from a cell value.
    Accepts comma/semicolon separated strings, lists, numbers, or NaN.
    """
    if pd.isna(cell):
        return []
    if isinstance(cell, (list, tuple)):
        items = cell
    else:
        items = re.split(r"[,;]", str(cell))
    wells = []
    for it in items:
        it = it.strip()
        if not it:
            continue
        m = re.match(r"^([A-Za-z])0*([0-9]+)$", it)
        if m:
            wells.append(m.group(1).upper() + str(int(m.group(2))))
        else:
            # keep as-is uppercase
            wells.append(it.upper())
    return wells


if not os.path.exists(excel_file):
    print(f"❌ Excel file not found at {excel_file}. Please put Raw_files.xlsx in {workdir}")
    sys.exit(1)

# Try reading the excel; header row can vary. First try header=2, else header=0
try:
    raw_df = pd.read_excel(excel_file, header=2)
    # if the columns aren't as expected, fallback
    if "Plate" not in raw_df.columns:
        raw_df = pd.read_excel(excel_file, header=0)
except Exception as e:
    print(f"❌ Failed to read Excel file: {e}")
    sys.exit(1)

# Normalize columns: ensure columns like '30','60','90','120' exist where possible
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
        if not pd.isna(row.get(cutoff)):  # if there's data for this cutoff
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

    # Extract plate ID and cutoff from the Excel file plates first
    valid_plates = []
    for _, row in raw_df.iterrows():
        for cutoff in CUTOFFS:
            if not pd.isna(row.get(cutoff)):  # if there's data for this cutoff
                plate = row["Plate"].strip()
                valid_plates.append(f"{plate}_{cutoff}.csv")

    # Only process files that exactly match plate_cutoff from Excel
    if file not in valid_plates:
        skipped += 1
        continue

    # Extract cutoff from filename
    m = re.search(r"_(30|60|90|120)\.csv$", file, re.IGNORECASE)
    if not m:  # This shouldn't happen given our valid_plates check
        skipped += 1
        continue

    cutoff = m.group(1)
    plate_id = file[:m.start()]
    cutoff_col = cutoff

# Try different plate ID formats in Raw_files.xlsx
found_row = None
plate_variants = [
    plate_id,  # original
    plate_id.replace("_", ""),  # without underscore
    re.sub(r'_\d+$', '', plate_id),  # without trailing _N
    re.sub(r'^(Full|Malt|Suc|Lac|Phlo|Iso|IMu)_?', '', plate_id),  # without prefix
]

for variant in plate_variants:
    row = raw_df.loc[raw_df["Plate"].str.strip() == variant.strip()]
    if not row.empty:
        found_row = row
        used_plate = variant
        break

if found_row is None:
    print(f"⚠️ No entry for plate '{plate_id}' (or variants) in Excel, skipping {file}")
    skipped += 1
    continue

saturated_cell = found_row.iloc[0].get(cutoff_col)
wells = parse_wells(saturated_cell)
if not wells:
    print(f"ℹ️ No saturated wells for plate '{used_plate}' at cutoff {cutoff}, skipping {file}.")
    skipped += 1
    continue    print(f"Processing {file}: {len(wells)} wells to clean: {wells}")

    csv_path = os.path.join(workdir, file)
    try:
        df = pd.read_csv(csv_path, dtype=object)
    except Exception as e:
        print(f"❌ Failed to read {file}: {e}")
        skipped += 1
        continue

    # make column names strings for easier matching
    df.columns = [str(c) for c in df.columns]

    row_col = find_row_column(df)
    if not row_col:
        print(f"⚠️ Could not find row-label column in {file}. Common names: 'Batch' or 'Row'. Skipping.")
        skipped += 1
        continue

    # apply cleaning
    changed_any = False
    for well in wells:
        if not well:
            continue
        m2 = re.match(r"^([A-Z])([0-9]+)$", well)
        if not m2:
            print(f"⚠️ Unrecognized well format '{well}' in {file}; expected like 'A1'. Skipping that entry.")
            continue
        row_letter = m2.group(1)
        col_number = m2.group(2)

        target_col = find_column_for_number(df, col_number)
        if not target_col:
            print(f"⚠️ Could not find column matching well column '{col_number}' in {file}.")
            continue

        mask = df[row_col].astype(str).str.strip().str.upper() == row_letter
        if mask.any():
            df.loc[mask, target_col] = pd.NA
            changed_any = True
        else:
            print(f"⚠️ No rows with {row_col}=={row_letter} found in {file}")

    if changed_any:
        new_name = file[:-4] + "_clean.csv"
        save_path = os.path.join(output_dir, new_name)
        try:
            df.to_csv(save_path, index=False)
            print(f"✅ Saved cleaned file: {save_path}")
            processed += 1
        except Exception as e:
            print(f"❌ Failed to save cleaned file for {file}: {e}")
            skipped += 1
    else:
        print(f"ℹ️ No changes made to {file} (no matching wells/rows).")
        skipped += 1

print(f"\nDone. Processed: {processed}. Skipped: {skipped}.")
