import pandas as pd
import numpy as np

# ========================== 1. ĐỌC FILE (đường dẫn đã sửa) ==========================
# Dùng raw string (r"....") hoặc dấu / để tránh lỗi \
df = pd.read_csv(r"C:\Users\nguye\OneDrive - National Economics University\Documents\KTL\Project\output\clean_data.csv")

print("Đã đọc file thành công! Số dòng:", len(df))

# ========================== 2. TẠO CỘT URBAN (Task 1) ==========================
urban_tinh = {1, 31, 48, 79, 92}   # Hà Nội(1), Hải Phòng(31), Đà Nẵng(48), TP.HCM(79), Cần Thơ(92)

df['Urban'] = df['TINH'].apply(
    lambda x: 1 if pd.notna(x) and int(x) in urban_tinh else 0
)

print("Số quan sát urban:", df['Urban'].sum())
print(df[['TINH', 'Urban']].head(10))

# ========================== 3. TẠO CỘT OCCUPATION_GROUP (Task 2) ==========================
occupation_map = {
    0: 'Lực lượng quân đội',
    1: 'Nhà lãnh đạo trong các ngành, các cấp và các đơn vị',
    2: 'Nhà chuyên môn bậc cao',
    3: 'Nhà chuyên môn bậc trung',
    4: 'Nhân viên trợ lý văn phòng',
    5: 'Nhân viên dịch vụ và bán hàng',
    6: 'Lao động có kỹ năng trong nông nghiệp, lâm nghiệp và thủy sản',
    7: 'Lao động thủ công và các nghề nghiệp có liên quan khác',
    8: 'Thợ lắp ráp và vận hành máy móc, thiết bị',
    9: 'Lao động giản đơn'
}

# Lấy 3 chữ số đầu của C30C
df['C30C_3digit'] = pd.to_numeric(df['C30C'].astype(str).str[:1], errors='coerce').fillna(0).astype(int)

df['Occupation_Group'] = df['C30C_3digit'].map(occupation_map)
df['Occupation_Group'] = df['Occupation_Group'].fillna('Không xác định / Mã ngoài danh mục')

print("\nVí dụ Occupation_Group:")
print(df[['C30C', 'C30C_3digit', 'Occupation_Group']].head(15))



# ========================== 4. LƯU FILE KẾT QUẢ ==========================
output_path = r"C:\Users\nguye\OneDrive - National Economics University\Documents\KTL\Project\output\clean_data_updated.csv"
df.to_csv(output_path, index=False, encoding='utf-8-sig')

print("\n✅ HOÀN THÀNH!")
print(f"Đã lưu file: {output_path}")
print("Các cột mới đã thêm:")
print("- Urban (0 = rural, 1 = urban)")
print("- Occupation_Group (tên nghề theo cấp 3 trong PDF)")
