# Xem tên cột và vài dòng đầu
df_raw <- read_dta("data/LFS_2018 (3)_cut_dup.dta", encoding = "CP1258")

names(df_raw)
head(df_raw, 10)

# Xem phân phối giá trị của C2
table(df_raw$C2, useNA = "ifany")

# ============================================================
# TẠO BIẾN: Grandparents_Presence
# Logic: = 1 nếu trong hộ có ÍT NHẤT 1 người có C2 = 4
#        = 0 nếu không có ai trong hộ có C2 = 4
# ============================================================

# BƯỚC 1: Tạo household_id duy nhất
# (Thay tên cột cho đúng với file thực tế của bạn)
# Thường LFS ghép từ: mã tỉnh + mã huyện + mã xã + số hộ

df_raw <- df_raw %>%
  mutate(
    hhid = paste(tinh, huyen, xa, hoso, sep = "_")
    # Nếu đã có sẵn cột hhid thì bỏ dòng này
  )

# BƯỚC 2: Với mỗi hộ, đánh dấu xem có ai có C2 = 4 không
grandparent_flag <- df_raw %>%
  group_by(hhid) %>%
  summarise(
    Grandparents_Presence = as.integer(any(C2 == 4, na.rm = TRUE)),
    .groups = "drop"
  )

# BƯỚC 3: Merge flag ngược lại vào toàn bộ df_raw
df_raw <- df_raw %>%
  left_join(grandparent_flag, by = "hhid")

# BƯỚC 4: Kiểm tra kết quả
cat("=== PHÂN PHỐI Grandparents_Presence ===\n")
table(df_raw$Grandparents_Presence, useNA = "ifany")

cat("\nTỷ lệ hộ có ông/bà:",
    round(mean(df_raw$Grandparents_Presence, na.rm = TRUE) * 100, 1), "%\n")

# Kiểm tra chéo: trong những hộ có C2=4, biến phải = 1
cat("\n--- Kiểm tra chéo ---\n")
df_raw %>%
  filter(C2 == 4) %>%
  count(Grandparents_Presence)
# → Tất cả phải là 1

# Trường hợp 1: C2 = 4 là "cháu" (grandCHILD), không phải "ông/bà"
# → Cần xác định lại: người GIÀ trong hộ = chủ hộ già + C2=4?
# Ví dụ an toàn hơn: kết hợp C2=4 VÀ tuổi >= 60

grandparent_flag_v2 <- df_raw %>%
  group_by(hhid) %>%
  summarise(
    Grandparents_Presence = as.integer(
      any(C2 == 4 & C5 >= 60, na.rm = TRUE)
    ),
    .groups = "drop"
  )

# Trường hợp 2: Nếu LFS không có C2 mà dùng tên cột khác
# (VD: "quanhe", "C02", "rel_head") → thay C2 bằng tên đúng

# Trong file clean_lfs2018.R, thêm vào phần tạo biến (Bước 8):
df <- df %>%
  left_join(grandparent_flag, by = "hhid")

# Và vào Probit Selection Equation:
# P(LFP=1) = Φ(Age, Age², Qualification, C9, C1, Grandparents_Presence, Urban)
