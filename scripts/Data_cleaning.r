# ============================================================
# SCRIPT: Tiền xử lý Dữ liệu LFS 2018
# Nghiên cứu: Decomposing the Gender Wage Gap in Vietnam
#             (Heckman-Oaxaca Approach)
# Author : [Tên bạn]
# Date   : 2025-04
# ============================================================
#
# BIẾN SỬ DỤNG (theo đúng tên cột LFS 2018):
#   C1  – Số thành viên hộ gia đình (Household Size)
#   C3  – Giới tính (1=Nam, 2=Nữ)
#   C5  – Tuổi (Age)
#   C9  – Tình trạng hôn nhân (Marital Status)
#   C17 – Trình độ học vấn cao nhất
#   C19 – Bằng cấp kỹ thuật / đào tạo nghề
#   C21/C22/C23 – Trạng thái tham gia thị trường lao động → LFP
#   C30 – Mã ngành (VSIC) → Industry_Group
#   C31 – Khu vực kinh tế (Sector: Nhà nước, Tư nhân, FDI…)
#   C44 – Thu nhập tháng (nghìn đồng)
#   C46 – Số giờ làm việc / tuần
#   urban_rural – Thành thị / Nông thôn (metadata của bảng hỏi)
# ============================================================


# ============================================================
# 0. CÀI ĐẶT & NẠP THƯ VIỆN
# ============================================================

# Chạy dòng này một lần nếu chưa cài:
# install.packages(c("haven", "dplyr", "ggplot2", "DescTools",
#                    "writexl", "readr", "tidyr"))

library(haven)      # Đọc .dta / .sav
library(dplyr)      # Thao tác dữ liệu
library(ggplot2)    # Vẽ biểu đồ
library(DescTools)  # Hàm Winsorize()
library(readr)      # Xuất CSV

# Tạo thư mục output
dir.create("output", showWarnings = FALSE)


# ============================================================
# 1. NẠP FILE DỮ LIỆU GỐC
# ============================================================

# --- Chỉnh đường dẫn cho đúng với máy bạn ---
# File .dta (Stata):
df_raw <- read_dta("data/LFS_2018 (3)_cut_dup.dta", encoding = "CP1258")

# File .sav (SPSS):
# df_raw <- read_spss("data/LFS_2018.sav")

# File .csv:
# df_raw <- read_csv("data/LFS_2018.csv")

cat("=== DỮ LIỆU GỐC ===\n")
cat("Số quan sát :", nrow(df_raw), "\n")
cat("Số biến     :", ncol(df_raw), "\n")

# Xem tên cột thực tế — đối chiếu trước khi chạy tiếp!
cat("\n--- Tên tất cả cột ---\n")
names(df_raw)


# ============================================================
# 2. GIỮ LẠI CÁC CỘT CẦN THIẾT
# ============================================================
# !! Quan trọng: kiểm tra names(df_raw) ở trên rồi thay
#    tên cột bên phải '=' cho khớp với file thực tế.
df <- df_raw %>%
  select(
    C3, C5, C9, C17, C19, C21, C22, C23,
    C30C,          # MÃ NGHỀ NGHIỆP 4 SỐ
    C31,
    C44, C46,
    Region,
    TINH
  )

cat("\n=== SAU KHI LỌC CỘT ===\n")
dim(df)


# ============================================================
# 3. LỌC MẪU BAN ĐẦU (KHÔNG GÂY TRUNCATION BIAS)
# ============================================================

# 3A. Giữ mẫu trong độ tuổi lao động [15, 65]
df <- df %>%
  filter(C5 >= 15 & C5 <= 65)

cat("Sau khi lọc tuổi 15-65     :", nrow(df), "quan sát\n")

# 3B. Chia ngay thành 2 mẫu con theo giới tính
#     (dùng cho cả Probit stage – toàn bộ; Wage stage – chỉ người đang làm)
df_male   <- df %>% filter(C3 == 1)
df_female <- df %>% filter(C3 == 2)

cat("  Mẫu Nam (C3=1)  :", nrow(df_male),   "\n")
cat("  Mẫu Nữ (C3=2)   :", nrow(df_female), "\n")


# ============================================================
# 4. TẠO BIẾN LFP (Lực lượng Lao động Tham gia)
# ============================================================
# LFP = 1 nếu đang làm việc (C21=1 HOẶC C22=1 HOẶC C23=1)
# LFP = 0 nếu không tham gia lực lượng lao động
#
# Logic: Xem codebook LFS để xác nhận mã = 1 là "có việc làm".
# Nếu LFS dùng mã khác (VD: C21=2), hãy điều chỉnh bên dưới.

df <- df %>%
  mutate(
    LFP = case_when(
      C21 == 1 | C22 == 1 | C23 == 1 ~ 1L,
      TRUE                             ~ 0L
    )
  )

cat("\n=== PHÂN PHỐI LFP ===\n")
print(table(df$LFP, useNA = "ifany"))
cat("Tỷ lệ tham gia LLLĐ :", round(mean(df$LFP, na.rm = TRUE)*100, 1), "%\n")


# ============================================================
# 5. LỌC NHIỄU QUA BIẾN ĐỘC LẬP (GIỜ LÀM VIỆC C46)
# ============================================================
# Chỉ áp dụng cho người ĐANG LÀM VIỆC (LFP=1)
# Lọc giờ cực đoan: < 10 hoặc > 90 giờ/tuần
# → Đây là lọc an toàn qua biến X, KHÔNG gây Truncation Bias
#   vì ta không xóa theo biến Y (lương)

df_workers <- df %>%
  filter(LFP == 1) %>%
  filter(!is.na(C46) & C46 >= 10 & C46 <= 90) %>%
  filter(!is.na(C44) & C44 > 0)    # Loại lương âm/bằng 0 (lỗi nhập liệu)

cat("\n=== SAU KHI LỌC GIỜ LÀM (CHỈ LFP=1) ===\n")
cat("Người đang làm việc (trước lọc giờ) :", sum(df$LFP == 1, na.rm = TRUE), "\n")
cat("Người đang làm việc (sau lọc giờ)   :", nrow(df_workers), "\n")


# ============================================================
# 6. TÍNH LƯƠNG GIỜ & GIAO THỨC KHÁM NGHIỆM NGOẠI LAI
# ============================================================

# Công thức theo đề cương:
#   Lương giờ = C44 / (C46 × 4.33)

df_workers <- df_workers %>%
  mutate(
    hourly_wage_raw = C44 / (C46 * 4.33)
  )

# -----------------------------------------------------------
# 6A. QUÉT PHÂN VỊ (Quantile Check)
# -----------------------------------------------------------
cat("\n=== QUANTILE CHECK – LƯƠNG GIỜ (TRƯỚC WINSORIZE) ===\n")
q_check <- quantile(df_workers$hourly_wage_raw,
                    probs = c(0.01, 0.05, 0.25, 0.50,
                              0.75, 0.95, 0.99, 0.999, 1.00),
                    na.rm = TRUE)
print(round(q_check, 1))

cat("\n→ Nhìn vào mốc 99%:")
cat("\n   Nếu ≈ 100-200 nghìn đ/giờ → hợp lý với thực tế VN 2018")
cat("\n   Nếu >> 500 nghìn đ/giờ   → có thể lỗi nhập liệu (thừa số 0)\n")
cat("→ Chốt mốc Winsorize: p1% =", round(q_check["1%"],1),
    " | p99% =", round(q_check["99%"],1), "\n")

# -----------------------------------------------------------
# 6B. SOI TRỰC QUAN (Boxplot TRƯỚC Winsorize)
# -----------------------------------------------------------
p_before <- ggplot(df_workers, aes(y = hourly_wage_raw)) +
  geom_boxplot(fill = "#4E9AF1", alpha = 0.65,
               outlier.colour = "black", outlier.size = 0.8,
               outlier.alpha = 0.5) +
  labs(
    title    = "Phân phối Lương giờ — TRƯỚC khi Winsorize",
    subtitle = "Chấm đen = ngoại lai đang bóp méo phân phối chung",
    y        = "Lương giờ (nghìn đồng)",
    caption  = "Nguồn: LFS 2018 | Tính theo C44/(C46×4.33)"
  ) +
  theme_minimal(base_size = 12)

ggsave("output/boxplot_before_winsorize.png",
       p_before, width = 5, height = 6, dpi = 300)
print(p_before)

# -----------------------------------------------------------
# 6C. ĐÁNH GIÁ TÁC ĐỘNG (Impact Assessment — Nháp)
# -----------------------------------------------------------
cat("\n=== IMPACT ASSESSMENT (NHÁP – TRƯỚC WINSORIZE) ===\n")
cat("Mean:", round(mean(df_workers$hourly_wage_raw, na.rm=TRUE), 2), "\n")
cat("SD  :", round(sd(df_workers$hourly_wage_raw,   na.rm=TRUE), 2), "\n")
cat("→ Mục tiêu: Sau Winsorize, SD giảm mạnh, Mean gần giữ nguyên\n")


# ============================================================
# 7. THỰC THI WINSORIZATION (ÉP BIÊN p1% – p99%)
# ============================================================
# QUAN TRỌNG: Dùng Winsorize() – KHÔNG dùng filter() để xóa dòng
#             → Tránh Truncation Bias (Thiên lệch cắt cụt)
# Hàm DescTools::Winsorize() ép giá trị về mức trần/sàn,
# GIỮ NGUYÊN số quan sát.

# Định nghĩa hàm winsorize đơn giản
winsorize_manual <- function(x, lower = 0.01, upper = 0.99, na.rm = TRUE) {
  q <- quantile(x, probs = c(lower, upper), na.rm = na.rm)
  x <- pmin(pmax(x, q[1]), q[2])
  return(x)
}

df_workers <- df_workers %>%
  mutate(
    hourly_wage = winsorize_manual(hourly_wage_raw, lower = 0.01, upper = 0.99)
  )

# Kiểm tra sau Winsorize
cat("\n=== IMPACT ASSESSMENT (SAU WINSORIZE) ===\n")
cat("Mean:", round(mean(df_workers$hourly_wage, na.rm=TRUE), 2), "\n")
cat("SD  :", round(sd(df_workers$hourly_wage,   na.rm=TRUE), 2), "\n")

# Boxplot SAU Winsorize
p_after <- ggplot(df_workers, aes(y = hourly_wage)) +
  geom_boxplot(fill = "#2ECC71", alpha = 0.65,
               outlier.colour = "darkred", outlier.size = 0.8) +
  labs(
    title    = "Phân phối Lương giờ — SAU khi Winsorize (p1%–p99%)",
    subtitle = "Ngoại lai đã được ép về mức trần/sàn, không bị xóa",
    y        = "Lương giờ (nghìn đồng)",
    caption  = "Nguồn: LFS 2018"
  ) +
  theme_minimal(base_size = 12)

ggsave("output/boxplot_after_winsorize.png",
       p_after, width = 5, height = 6, dpi = 300)
print(p_after)


# ============================================================
# 8. TẠO CÁC BIẾN MỚI THEO MÔ HÌNH
# ============================================================
# --- 8A. Biến mới cho người ĐI LÀM (df_workers) ---
df_workers <- df_workers %>%
  mutate(
    ln_Hourly_Wage = log(hourly_wage),
    Age_Squared    = C5^2,
    
    Highest_Qualification = case_when(
      C17 >= 7 ~ "Postgraduate",
      C17 == 6 ~ "University",
      C17 == 5 | C19 == 3 ~ "College",
      C17 == 4 | C19 == 2 ~ "Vocational_Upper",
      C19 == 1 ~ "Vocational_Lower",
      C17 == 3 ~ "Upper_Secondary",
      C17 == 2 ~ "Lower_Secondary",
      C17 == 1 ~ "Primary",
      TRUE ~ "No_Qualification"
    ),
    Highest_Qualification = factor(Highest_Qualification,
                                   levels = c("No_Qualification", "Primary", "Lower_Secondary",
                                              "Upper_Secondary", "Vocational_Lower", "Vocational_Upper",
                                              "College", "University", "Postgraduate")),
    
    # ==================== OCCUPATION_GROUP (Nhóm nghề rộng - dùng cho phân tích phân mảnh) ====================
    # Dựa trên chữ số đầu tiên của mã nghề C30C (theo chuẩn ISCO-08)
    Occupation_Group = case_when(
      C30C >= 1000 & C30C <= 1999 ~ "Managers_Leaders",           # Cấp 1 = 1
      C30C >= 2000 & C30C <= 2999 ~ "Professionals",              # Cấp 1 = 2
      C30C >= 3000 & C30C <= 3999 ~ "Technicians_Associate",      # Cấp 1 = 3
      C30C >= 4000 & C30C <= 4999 ~ "Clerical_Support",           # Cấp 1 = 4
      C30C >= 5000 & C30C <= 5999 ~ "Service_Sales",              # Cấp 1 = 5
      C30C >= 6000 & C30C <= 6999 ~ "Skilled_Agricultural",       # Cấp 1 = 6
      C30C >= 7000 & C30C <= 7999 ~ "Craft_Trades",               # Cấp 1 = 7
      C30C >= 8000 & C30C <= 8999 ~ "Operators_Assemblers",       # Cấp 1 = 8
      C30C >= 9000 & C30C <= 9999 ~ "Elementary",                 # Cấp 1 = 9
      C30C >= 0    & C30C <= 999  ~ "Armed_Forces",               # Cấp 1 = 0 (quân đội)
      TRUE                        ~ "Other"
    ),
    Occupation_Group = factor(Occupation_Group,
                              levels = c("Managers_Leaders", "Professionals", "Technicians_Associate",
                                         "Clerical_Support", "Service_Sales", "Skilled_Agricultural",
                                         "Craft_Trades", "Operators_Assemblers", "Elementary",
                                         "Armed_Forces", "Other")),
    
    # ==================== OCCUPATION_CODE (Mã nghề gốc 4 số - giữ nguyên để phân tích sâu) ====================
    Occupation_Code = C30C,   # Lưu trực tiếp mã 4 chữ số, tương ứng với từng dòng
    
    # ==================== INDUSTRY_GROUP (Phân ngành theo logic người dùng yêu cầu) ====================
    # LƯU Ý: Logic này sử dụng cột C30C (mã nghề) thay vì mã ngành thực tế (như C31 hay VSIC).
    # Bạn đã yêu cầu cụ thể phân loại theo các khoảng giá trị của C30C như dưới đây.
    Industry_Group = case_when(
      C30C >= 100 & C30C <= 399   ~ "Agriculture",              # Nông nghiệp
      C30C >= 500 & C30C <= 4399  ~ "Industry_Construction",    # Công nghiệp & Xây dựng
      C30C >= 4500                ~ "Services",                 # Dịch vụ
      TRUE                        ~ "Other"
    ),
    Industry_Group = factor(Industry_Group,
                            levels = c("Agriculture", "Industry_Construction", "Services", "Other")),
    # ==================================================================================================
    
    Sector = case_when(
      C31 == 1 ~ "State",
      C31 == 4 ~ "FDI",
      C31 %in% c(2, 3) ~ "Private",
      TRUE ~ "Other"
    ),
    Sector = factor(Sector, levels = c("State", "Private", "FDI", "Other")),
    
    Urban = case_when(
      Region == 1 ~ 1L,
      Region == 2 ~ 0L,
      TRUE ~ NA_integer_
    ),
    Gender = factor(C3, levels = c(1, 2), labels = c("Nam", "Nữ"))
  )


# --- 8B. Biến mới cho TOÀN MẪU df (dùng cho Probit stage) ---
df <- df %>%
  mutate(
    Age_Squared = C5^2,
    
    Highest_Qualification = case_when(
      C17 >= 7                           ~ "Postgraduate",
      C17 == 6                           ~ "University",
      C17 == 5 | C19 == 3                ~ "College",
      C17 == 4 | C19 == 2                ~ "Vocational_Upper",
      C19 == 1                           ~ "Vocational_Lower",
      C17 == 3                           ~ "Upper_Secondary",
      C17 == 2                           ~ "Lower_Secondary",
      C17 == 1                           ~ "Primary",
      TRUE                               ~ "No_Qualification"
    ),
    Highest_Qualification = factor(
      Highest_Qualification,
      levels = c("No_Qualification", "Primary", "Lower_Secondary",
                 "Upper_Secondary", "Vocational_Lower", "Vocational_Upper",
                 "College", "University", "Postgraduate")
    ),
    
    Marital_Status = case_when(
      C9 == 1 ~ "Never_Married",
      C9 == 2 ~ "Married",
      C9 == 3 ~ "Divorced_Separated",
      C9 == 4 ~ "Widowed",
      TRUE    ~ NA_character_
    ),
    Marital_Status = factor(Marital_Status),
    
    # Household Size: C1 dùng trực tiếp (biến liên tục)
    #Household_Size = C1,
    
    #Urban = case_when(
     # urban_rural == 1 ~ 1L,
      #urban_rural == 2 ~ 0L,
      #TRUE             ~ NA_integer_
    #),
    
    Gender = factor(C3, levels = c(1, 2), labels = c("Nam", "Nữ"))
  )

cat("\n=== KIỂM TRA BIẾN MỚI (df_workers) ===\n")
summary(df_workers %>%
          select(ln_Hourly_Wage, Age_Squared, Highest_Qualification,
                 Industry_Group, Sector, Urban, Gender))


# ============================================================
# 9. XUẤT FILE SẠCH
# ============================================================

# --- 9A. clean_data.csv: toàn mẫu (Probit stage) ---
write_csv(df, "output/clean_data.csv")
cat("\n✓ Đã xuất: output/clean_data.csv\n")
cat("  Quan sát:", nrow(df), "| Biến:", ncol(df), "\n")

# --- 9B. clean_data_workers.csv: chỉ người đi làm (Wage stage) ---
write_csv(df_workers, "output/clean_data_workers.csv")
cat("✓ Đã xuất: output/clean_data_workers.csv\n")
cat("  Quan sát:", nrow(df_workers), "| Biến:", ncol(df_workers), "\n")


# ============================================================
# 10. THỐNG KÊ MÔ TẢ THEO GIỚI TÍNH (CHO BÁO CÁO)
# ============================================================
# Bảng 1: Toàn mẫu (Probit stage — tất cả 15-65 tuổi)
# Bảng 2: Mẫu người đi làm (Wage stage — LFP=1)

# --- Hàm tính Mean và SD gọn ---
desc_fn <- function(data) {
  data %>%
    group_by(Gender) %>%
    summarise(
      N = n(),
      
      # Lương (chỉ có ở workers)
      Mean_Hourly_Wage  = if("hourly_wage"    %in% names(.)) mean(hourly_wage,    na.rm=TRUE) else NA,
      SD_Hourly_Wage    = if("hourly_wage"    %in% names(.)) sd(hourly_wage,      na.rm=TRUE) else NA,
      Mean_ln_Wage      = if("ln_Hourly_Wage" %in% names(.)) mean(ln_Hourly_Wage, na.rm=TRUE) else NA,
      SD_ln_Wage        = if("ln_Hourly_Wage" %in% names(.)) sd(ln_Hourly_Wage,   na.rm=TRUE) else NA,
      
      # Tuổi
      Mean_Age          = mean(C5,            na.rm=TRUE),
      SD_Age            = sd(C5,              na.rm=TRUE),
      Mean_Age_Squared  = mean(Age_Squared,   na.rm=TRUE),
      
      # Giờ làm (chỉ có ở workers)
      Mean_Hours        = if("C46" %in% names(.)) mean(C46, na.rm=TRUE) else NA,
      SD_Hours          = if("C46" %in% names(.)) sd(C46,   na.rm=TRUE) else NA,
      
      # Tham gia LLLĐ
      LFP_Rate_pct      = if("LFP" %in% names(.)) mean(LFP, na.rm=TRUE)*100 else NA,
      
      # Hộ gia đình
      #Mean_HHSize       = mean(C1, na.rm=TRUE),
      #SD_HHSize         = sd(C1,   na.rm=TRUE),
      
      # Thành thị
      #Urban_Rate_pct    = mean(Urban, na.rm=TRUE)*100,
      
      .groups = "drop"
    ) %>%
    mutate(across(where(is.numeric), ~round(., 3)))
}

# Bảng 1: Toàn mẫu (cho Probit)
stats_all     <- desc_fn(df)
# Bảng 2: Mẫu đi làm (cho Wage Equation)
stats_workers <- desc_fn(df_workers)

cat("\n=== BẢNG THỐNG KÊ MÔ TẢ — TOÀN MẪU (Probit stage) ===\n")
print(as.data.frame(stats_all))

cat("\n=== BẢNG THỐNG KÊ MÔ TẢ — NGƯỜI ĐI LÀM (Wage stage) ===\n")
print(as.data.frame(stats_workers))

# Xuất bảng thống kê
write_csv(stats_all,     "output/desc_stats_full_sample.csv")
write_csv(stats_workers, "output/desc_stats_workers.csv")
cat("\n✓ Đã xuất: output/desc_stats_full_sample.csv\n")
cat("✓ Đã xuất: output/desc_stats_workers.csv\n")


# ============================================================
# 11. THỐNG KÊ MÔ TẢ NGÀNH & TRÌNH ĐỘ HỌC VẤN
# ============================================================

# Phân phối ngành theo giới tính (%)
cat("\n=== PHÂN PHỐI NGÀNH THEO GIỚI TÍNH (%) ===\n")
industry_tab <- df_workers %>%
  count(Gender, Industry_Group) %>%
  group_by(Gender) %>%
  mutate(Pct = round(n / sum(n) * 100, 1)) %>%
  select(-n) %>%
  tidyr::pivot_wider(names_from = Gender, values_from = Pct)
print(industry_tab)
write_csv(industry_tab, "output/industry_by_gender.csv")

# Phân phối học vấn theo giới tính (%)
cat("\n=== PHÂN PHỐI HỌC VẤN THEO GIỚI TÍNH (%) ===\n")
edu_tab <- df_workers %>%
  count(Gender, Highest_Qualification) %>%
  group_by(Gender) %>%
  mutate(Pct = round(n / sum(n) * 100, 1)) %>%
  select(-n) %>%
  tidyr::pivot_wider(names_from = Gender, values_from = Pct)
print(edu_tab)
write_csv(edu_tab, "output/education_by_gender.csv")


# ============================================================
# 12. TÓM TẮT CẤU TRÚC MẪU CON
# ============================================================

cat("\n=== CẤU TRÚC MẪU CON CHO HECKMAN ===\n")
cat("Mẫu NAM:\n")
cat("  Probit stage (all males 15-65)  :", sum(df$C3 == 1, na.rm=TRUE), "\n")
cat("  Wage stage   (working males)    :", sum(df_workers$C3 == 1, na.rm=TRUE), "\n")

cat("Mẫu NỮ:\n")
cat("  Probit stage (all females 15-65):", sum(df$C3 == 2, na.rm=TRUE), "\n")
cat("  Wage stage   (working females)  :", sum(df_workers$C3 == 2, na.rm=TRUE), "\n")

cat("\n=== HOÀN THÀNH ===\n")
cat("Các file trong thư mục output/:\n")
cat("  clean_data.csv             → Đẩy lên GitHub / Drive\n")
cat("  clean_data_workers.csv     → Dùng cho Wage Equation\n")
cat("  desc_stats_full_sample.csv → Bảng mô tả toàn mẫu (báo cáo)\n")
cat("  desc_stats_workers.csv     → Bảng mô tả người đi làm (báo cáo)\n")
cat("  industry_by_gender.csv     → Phân phối ngành\n")
cat("  education_by_gender.csv    → Phân phối học vấn\n")
cat("  boxplot_before_winsorize.png → Slide thuyết trình\n")
cat("  boxplot_after_winsorize.png  → Slide thuyết trình\n")