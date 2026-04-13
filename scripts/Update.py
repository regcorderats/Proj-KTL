# ============================================================
# SCRIPT: Tạo biến Grandparents_Presence từ file SQL output
# Nghiên cứu: Gender Wage Gap in Vietnam — Heckman-Oaxaca
# ============================================================

import pandas as pd
import os

# ============================================================
# 0. CẤU HÌNH — CHỈNH Ở ĐÂY
# ============================================================

# ✅ ĐÃ SỬA: Dùng raw string (r"....") để Python không hiểu \U là Unicode
INPUT_FILE  = r"C:\Users\nguye\OneDrive - National Economics University\Documents\KTL\Project\output\clean_data_updated.csv"

OUTPUT_FILE = "output/clean_data_with_grandparent.xlsx"
OUTPUT_CSV  = "output/clean_data_with_grandparent.csv"

# Tên cột
COL_TINH   = "TINH"
COL_HUYEN  = "HUYEN"
COL_XA     = "XA"
COL_HOSO   = "HOSO"
COL_C2     = "C2"          # Quan hệ với chủ hộ (4 = ông/bà)
COL_C5     = "C5"          # Tuổi

GRANDPARENT_CODE = 4

os.makedirs("output", exist_ok=True)

# ============================================================
# 1. ĐỌC FILE (đã sửa read_csv thay vì read_excel)
# ============================================================
print("=" * 60)
print("ĐỌC FILE")
print("=" * 60)

df = pd.read_csv(INPUT_FILE)      # ← ĐÃ SỬA: read_csv vì file là .csv

print(f"Số dòng   : {len(df):,}")
print(f"Số cột    : {df.shape[1]}")
print(f"Tên cột:\n{list(df.columns)}")

# ============================================================
# 2. KIỂM TRA C2
# ============================================================
print("\n" + "=" * 60)
print("KIỂM TRA PHÂN PHỐI C2 (QUAN HỆ VỚI CHỦ HỘ)")
print("=" * 60)

print(df[COL_C2].value_counts(dropna=False).sort_index())

n_grandparents = (df[COL_C2] == GRANDPARENT_CODE).sum()
print(f"\nSố người có C2 = {GRANDPARENT_CODE} (ông/bà): {n_grandparents:,}")

# ============================================================
# 3. TẠO HOUSEHOLD_ID
# ============================================================
print("\n" + "=" * 60)
print("TẠO HOUSEHOLD_ID")
print("=" * 60)

id_cols = [col for col in [COL_TINH, COL_HUYEN, COL_XA, COL_HOSO] if col in df.columns]

if not id_cols:
    raise ValueError("Không tìm thấy cột nào để tạo household_id!")

print(f"Ghép household_id từ: {id_cols}")

df["household_id"] = df[id_cols].astype(str).agg("_".join, axis=1)
print(f"Số hộ duy nhất: {df['household_id'].nunique():,}")

# ============================================================
# 4. TẠO BIẾN Grandparents_Presence
# ============================================================
print("\n" + "=" * 60)
print("TẠO BIẾN Grandparents_Presence")
print("=" * 60)

df["Grandparents_Presence"] = (
    df.groupby("household_id")[COL_C2]
      .transform(lambda x: (x == GRANDPARENT_CODE).any())
      .astype(int)
)

# ============================================================
# 5. Phiên bản Strict (C2=4 VÀ tuổi >= 60)
# ============================================================
if COL_C5 in df.columns:
    print("✓ Đang tạo Grandparents_Presence_Strict (C2=4 & tuổi ≥ 60)...")
    
    strict = (
        df.groupby("household_id")
          .apply(lambda hh: ((hh[COL_C2] == GRANDPARENT_CODE) & (hh[COL_C5] >= 60)).any())
          .astype(int)
          .reset_index(name="Grandparents_Presence_Strict")
    )
    
    df = df.merge(strict, on="household_id", how="left")
    print("✓ Đã tạo Grandparents_Presence_Strict")
else:
    print(f"[!] Không tìm thấy cột tuổi '{COL_C5}' → bỏ qua phiên bản Strict")

# ============================================================
# 6. KIỂM TRA & XUẤT FILE
# ============================================================
print("\n" + "=" * 60)
print("KIỂM TRA KẾT QUẢ")
print("=" * 60)

print("\nPhân phối Grandparents_Presence:")
print(df["Grandparents_Presence"].value_counts().sort_index())
print(f"→ Tỷ lệ thành viên sống trong hộ có ông/bà: {df['Grandparents_Presence'].mean()*100:.1f}%")

hh_level = df.drop_duplicates("household_id")["Grandparents_Presence"]
print(f"→ Tỷ lệ HỘ có ông/bà: {hh_level.mean()*100:.1f}%")

if (df[df[COL_C2] == GRANDPARENT_CODE]["Grandparents_Presence"] == 1).all():
    print("✓ PASS: Tất cả ông/bà đều có flag = 1")

print("\n" + "=" * 60)
print("XUẤT FILE")
print("=" * 60)

df.to_excel(OUTPUT_FILE, index=False)
df.to_csv(OUTPUT_CSV, index=False, encoding="utf-8-sig")

print(f"✓ Đã xuất: {OUTPUT_FILE}")
print(f"✓ Đã xuất: {OUTPUT_CSV}")

# Xuất bảng cấp hộ (dùng để merge sau)
hh_table = df.groupby("household_id").agg({
    "Grandparents_Presence": "first",
    **({"Grandparents_Presence_Strict": "first"} if "Grandparents_Presence_Strict" in df.columns else {})
}).reset_index()

hh_table.to_csv("output/grandparent_flag_by_household.csv", index=False, encoding="utf-8-sig")
print(f"\n✓ Đã xuất: output/grandparent_flag_by_household.csv (mỗi hộ 1 dòng)")

print("\n=== HOÀN THÀNH ===")