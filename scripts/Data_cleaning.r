# ============================================================
# SCRIPT: Tiền xử lý Dữ liệu LFS 2018
# Nghiên cứu: Decomposing the Gender Wage Gap in Vietnam
#             (Heckman-Oaxaca Approach)
# ============================================================

# ============================================================
# 0. TỰ ĐỘNG CÀI ĐẶT & NẠP THƯ VIỆN
# ============================================================
# Danh sách các gói (packages) cần thiết cho toàn bộ dự án
required_packages <- c(
  "haven",      # Đọc file dta của Stata
  "dplyr",      # Xử lý dữ liệu
  "ggplot2",    # Vẽ biểu đồ
  "DescTools",  # Tính toán thống kê (Winsorize)
  "readr",      # Xuất/Nhập file csv tốc độ cao
  "tidyr",      # Dọn dẹp dữ liệu (drop_na)
  "car"         # Chạy kiểm định linearHypothesis cho OLS/Probit
)

# Kiểm tra máy tính hiện tại đã cài các gói này chưa, nếu chưa thì tự động tải về
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) {
  cat("\nĐang cài đặt các thư viện còn thiếu:", paste(new_packages, collapse = ", "), "\n")
  install.packages(new_packages, dependencies = TRUE)
}

# Nạp tất cả thư viện
invisible(lapply(required_packages, library, character.only = TRUE))

# Tự động tạo thư mục output nếu chưa tồn tại
dir.create("output", showWarnings = FALSE)


# ============================================================
# 1. NẠP FILE DỮ LIỆU GỐC (DÙNG ĐƯỜNG DẪN TƯƠNG ĐỐI)
# ============================================================
# Khai báo đường dẫn tương đối (File script và folder 'data' phải nằm cùng 1 chỗ)
data_path <- "data/LFS_2018.dta"

# Bẫy lỗi bảo vệ: Cảnh báo người dùng nếu họ quên bỏ file dta vào đúng chỗ
if (!file.exists(data_path)) {
  stop("LỖI CHÍ MẠNG: Không tìm thấy dữ liệu! Vui lòng copy file LFS_2018.dta vào thư mục 'data/' nằm cùng chỗ với script này.")
}

df_raw <- read_dta(data_path, encoding = "CP1258")

cat("=== DỮ LIỆU GỐC ===\n")
cat("Số quan sát :", nrow(df_raw), "\n")
cat("Số biến     :", ncol(df_raw), "\n")

# ... (GIỮ NGUYÊN TỪ PHẦN "df_hh_level <- df_raw %>%" CỦA BẠN TRỞ ĐI) ...

df_hh_level <- df_raw %>%
  mutate(
    ID_HO_DUY_NHAT = paste(TINH, HUYEN, XA, THANGDT, HOSO, sep = "_"),
    Is_Child_Under_6 = if_else(C5 < 6, 1, 0),
    Is_Grandparent = if_else(C2 == 4 | (C3 == 1 & C5 >= 60) | (C3 == 2 & C5 >= 55), 1, 0),
    Is_Inactive = if_else(C21 == 2 & C22 == 2 & C23 == 2 & (!is.na(C24) & C24 != 1), 1, 0),
    Is_Available_GP = if_else(Is_Grandparent == 1 & Is_Inactive == 1, 1, 0)
  ) %>%
  group_by(ID_HO_DUY_NHAT) %>%
  summarise(
    Tong_So_Thanh_Vien = n(),                             
    Has_Child_Under_6  = if_else(sum(Is_Child_Under_6, na.rm = TRUE) > 0, 1, 0),
    Has_Available_GP   = if_else(sum(Is_Available_GP, na.rm = TRUE) > 0, 1, 0),
    .groups = "drop"
  ) %>%
  select(ID_HO_DUY_NHAT, Has_Available_GP, Has_Child_Under_6)

# ============================================================
# 2. GIỮ LẠI CÁC CỘT CẦN THIẾT VÀ GHÉP BIẾN CÔNG CỤ
# ============================================================
df <- df_raw %>%
  zap_labels() %>%
  mutate(
    ID_CA_NHAN = row_number(),
    # Bắt buộc tạo lại ID này ở đây để làm key ghép nối (merge)
    ID_HO_DUY_NHAT = paste(TINH, HUYEN, XA, THANGDT, HOSO, sep = "_")
  ) %>%
  select(
    ID_CA_NHAN,
    ID_HO_DUY_NHAT, # Khóa ghép nối
    C3, C5, C9, C17, C19, C21, C22, C23,
    C30C, C29C,     
    C31, C44, C46, TTNT, TINH
  ) %>%
  # Ghép biến công cụ cấp hộ gia đình vào tập dữ liệu cá nhân
  left_join(df_hh_level, by = "ID_HO_DUY_NHAT")

cat("\n=== SAU KHI GHÉP BIẾN CÔNG CỤ VÀ LỌC CỘT ===\n")
dim(df)

# ============================================================
# 3. LỌC MẪU BAN ĐẦU (ĐỘ TUỔI CORE WORKING AGE)
# ============================================================
# Do bạn muốn cắt sinh viên và người nghỉ hưu để cô lập "Family Gap", 
# chạy Heckman trên độ tuổi 25-55 (Core Working Age) là hợp lý nhất.

df <- df %>%
  filter(C5 >= 25 & C5 <= 55)

cat("Sau khi lọc Core Working Age (25-55) :", nrow(df), "quan sát\n")


# ============================================================
# 4. TẠO BIẾN LFP (Lực lượng Lao động Tham gia)
# ============================================================
df <- df %>%
  mutate(
    LFP = case_when(
      C21 == 1 | C22 == 1 | C23 == 1 ~ 1L,
      TRUE                           ~ 0L
    )
  )


# ============================================================
# 5. LỌC NHIỄU QUA BIẾN ĐỘC LẬP (GIỜ LÀM VIỆC C46)
# ============================================================
df_workers <- df %>%
  filter(LFP == 1) %>%
  filter(!is.na(C46) & C46 >= 10 & C46 <= 90) %>%
  filter(!is.na(C44) & C44 > 0)    
  

# ============================================================
# 6 & 7. TÍNH LƯƠNG GIỜ & WINSORIZATION
# ============================================================
df_workers <- df_workers %>%
  mutate(
    hourly_wage_raw = C44 / (C46 * 4.33)
  )

winsorize_manual <- function(x, lower = 0.01, upper = 0.99, na.rm = TRUE) {
  q <- quantile(x, probs = c(lower, upper), na.rm = na.rm)
  x <- pmin(pmax(x, q[1]), q[2])
  return(x)
}

df_workers <- df_workers %>%
  mutate(
    hourly_wage = winsorize_manual(hourly_wage_raw, lower = 0.01, upper = 0.99)
  )


# ============================================================
# 8. TẠO CÁC BIẾN MỚI THEO MÔ HÌNH (GỘP CHO CẢ 2 BẢNG)
# ============================================================
# Chuyển function tạo biến ra ngoài để áp dụng đồng nhất cho cả df và df_workers
# để đảm bảo phương trình Selection (dùng df) và Wage (dùng df_workers) chung 1 gốc.

create_model_vars <- function(data) {
  data %>%
    mutate(
      # 1. Tuổi kinh nghiệm và Mincer
      Potential_Experience = C5 - case_when( 
        C17 >= 7 ~ 18,
        C17 == 6 ~ 16,
        C17 == 5 | C19 == 3 ~ 14,
        C17 == 4 | C19 == 2 ~ 12,
        C17 == 3 ~ 12,
        C17 == 2 ~ 9,
        C17 == 1 ~ 5,
        TRUE ~ 0
      ) - 6,
      Potential_Experience = if_else(Potential_Experience < 0, 0, Potential_Experience),
      Experience_Squared   = Potential_Experience^2,
      
      # 2. Học vấn
      Highest_Qualification = case_when(
        C17 >= 6 ~ "University_Plus",      # Gộp ĐH và Sau ĐH
        C17 == 4 | C17 == 5 | C19 >= 2 ~ "Vocational_College", # Cao đẳng/Trung cấp
        C17 == 3 ~ "Upper_Secondary",      # Cấp 3
        TRUE ~ "Lower_Secondary_Below"     # Cấp 2 trở xuống
      ),
      # ĐẶT BASE CHUẨN LÀ NHÓM THẤP NHẤT
      Highest_Qualification = factor(Highest_Qualification,
                                     levels = c("Lower_Secondary_Below", "Upper_Secondary", 
                                                "Vocational_College", "University_Plus")),
      
      # ĐẶT BASE CHUẨN LÀ ELEMENTARY (LAO ĐỘNG GIẢN ĐƠN) ĐỂ DỄ GIẢI THÍCH HỆ SỐ DƯƠNG
      
      # 3. Giới tính
      Gender = factor(C3, levels = c(1, 2), labels = c("Nam", "Nữ")),
      
      # ==========================================
      # 4. CÁC BIẾN KIỂM SOÁT BỊ THIẾU ĐÃ ĐƯỢC BỔ SUNG
      # ==========================================
      # Vùng miền (Thành thị/Nông thôn) - Dựa vào biến Region của VHLSS
      Urban = case_when(
        TTNT == 1 ~ 1L,  # Thành thị
        TTNT == 2 ~ 0L,  # Nông thôn
        TRUE ~ NA_integer_
      ),
      VUNG = case_when(
        TINH %in% c(1, 22, 24, 26, 27, 30, 31, 33, 34, 35, 36) ~ "Dong_Bang_Song_Hong",
        TINH %in% c(2, 4, 6, 8, 10, 11, 12, 14, 15, 17, 19, 20, 25) ~ "Trung_Du_Mien_Nui_Phia_Bac",
        TINH %in% c(38, 40, 42, 44, 45, 46, 48, 49, 51, 52, 54, 56, 58, 60) ~ "Bac_Trung_Bo_Duyen_Hai_MT",
        TINH %in% c(62, 64, 66, 67, 68) ~ "Tay_Nguyen",
        TINH %in% c(70, 74, 75, 77, 79, 82) ~ "Dong_Nam_Bo",
        TINH %in% c(80, 83, 84, 86, 87, 89, 91, 92, 93, 94, 95, 96) ~ "Dong_Bang_Song_Cuu_Long",
        TRUE ~ NA_character_
      ),
      
      # Đặt base chuẩn cho VUNG (thường lấy Đồng bằng sông Hồng làm nhóm tham chiếu)
      VUNG = factor(VUNG, levels = c("Dong_Bang_Song_Hong", "Trung_Du_Mien_Nui_Phia_Bac", 
                                     "Bac_Trung_Bo_Duyen_Hai_MT", "Tay_Nguyen", 
                                     "Dong_Nam_Bo", "Dong_Bang_Song_Cuu_Long")),
      
      # Tình trạng hôn nhân - Dựa vào C9
      Marital_Status = case_when(
        C9 == 1 ~ "Never_Married",
        C9 == 2 ~ "Married",
        C9 == 3 ~ "Divorced_Separated",
        C9 == 4 ~ "Widowed",
        TRUE    ~ NA_character_
      ),
      # ĐẶT BASE CHUẨN LÀ NEVER MARRIED
      Marital_Status = factor(Marital_Status, 
                              levels = c("Never_Married", "Married", "Divorced_Separated", "Widowed")),)
    
}

df <- df %>% create_model_vars()
df_workers <- df_workers %>% 
  create_model_vars() %>%
  mutate(
    ln_Hourly_Wage = log(hourly_wage),
    
    Occupation_Group = case_when(
      C29C >= 1000 & C29C <= 1999 ~ "Managers_Leaders",
      C29C >= 2000 & C29C <= 2999 ~ "Professionals",
      C29C >= 3000 & C29C <= 3999 ~ "Technicians_Associate",
      C29C >= 4000 & C29C <= 4999 ~ "Clerical_Support",
      C29C >= 5000 & C29C <= 5999 ~ "Service_Sales",
      C29C >= 6000 & C29C <= 6999 ~ "Skilled_Agricultural",
      C29C >= 7000 & C29C <= 7999 ~ "Craft_Trades",
      C29C >= 8000 & C29C <= 8999 ~ "Operators_Assemblers",
      C29C >= 9000 & C29C <= 9999 ~ "Elementary",
      C29C >= 0    & C29C <= 999  ~ "Armed_Forces",
      TRUE                        ~ "Other"
    ),
    Occupation_Group = factor(Occupation_Group,
                              levels = c("Elementary", "Skilled_Agricultural", "Craft_Trades", 
                                         "Operators_Assemblers", "Service_Sales", "Clerical_Support", 
                                         "Technicians_Associate", "Professionals", "Managers_Leaders", 
                                         "Armed_Forces", "Other")),
    Industry_Group = case_when(
      C30C >= 100 & C30C <= 399   ~ "Agriculture",
      C30C >= 500 & C30C <= 4399  ~ "Industry_Construction",
      C30C >= 4500                ~ "Services",
      TRUE                        ~ "Other"
    ),
    Sector = case_when(
      C31 == 1 ~ "State",
      C31 == 4 ~ "FDI",
      C31 %in% c(2, 3) ~ "Private",
      TRUE ~ "Other"
    )
  )
# ── Bước 0 (chạy 1 lần duy nhất, trước tất cả): đánh index gốc ───
df <- df %>% mutate(.row_id = row_number())

# ── Tạo df_female như cũ ──────────────────────────────────────────
df_female <- df %>% filter(C3 == 2)
df_male   <- df %>% filter(C3 == 1)

cat("\n=== CẤU TRÚC MẪU PROBIT ĐÃ FULL BIẾN ===\n")
cat("  Mẫu Nam (C3=1)  :", nrow(df_male),   "\n")
cat("  Mẫu Nữ (C3=2)   :", nrow(df_female), "\n")

# ── Sau block create_model_vars và mutate Occupation/Industry ─────

# Tạo 2-digit codes
# ── Tạo fine occupation/industry categories ───────────────────────
df_workers <- df_workers %>%
  mutate(
    Occ_2digit = factor(floor(C29C / 100)),
    Ind_division = floor(C30C / 100),
    Ind_section = case_when(
      Ind_division %in% 1:3       ~ "A_Agriculture",
      Ind_division %in% 5:9       ~ "B_Mining",
      Ind_division %in% 10:33     ~ "C_Manufacturing",
      Ind_division == 35          ~ "D_Electricity",
      Ind_division %in% 36:39     ~ "E_Water_Waste",
      Ind_division %in% 41:43     ~ "F_Construction",
      Ind_division %in% 45:47     ~ "G_Trade",
      Ind_division %in% 49:53     ~ "H_Transport",
      Ind_division %in% 55:56     ~ "I_Accommodation",
      Ind_division %in% 58:63     ~ "J_Information",
      Ind_division %in% 64:66     ~ "K_Finance",
      Ind_division == 68          ~ "L_RealEstate",
      Ind_division %in% 69:75     ~ "M_Professional",
      Ind_division %in% 77:82     ~ "N_Administrative",
      Ind_division == 84          ~ "O_PublicAdmin",
      Ind_division == 85          ~ "P_Education",
      Ind_division %in% 86:88     ~ "Q_Health",
      Ind_division %in% 90:93     ~ "R_Arts",
      Ind_division %in% 94:96     ~ "S_OtherServices",
      Ind_division %in% 97:98     ~ "T_Households",
      TRUE                        ~ "Other"
    ),
    Ind_section = factor(Ind_section)
  )

# Lọc thin cells cho Occ_2digit
valid_occ <- df_workers %>%
  group_by(Gender, Occ_2digit) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Occ_2digit) %>%
  summarise(min_n = min(n)) %>%
  filter(min_n >= 30) %>%
  pull(Occ_2digit)

df_workers <- df_workers %>%
  mutate(
    Occ_2digit_clean = if_else(Occ_2digit %in% valid_occ,
                               as.character(Occ_2digit), "Other"),
    Occ_2digit_clean = if_else(nchar(Occ_2digit_clean) > 4,
                               "Other", Occ_2digit_clean),
    Occ_2digit_clean = factor(Occ_2digit_clean)
  )

# Verify
cat("Occ 2digit categories:", nlevels(df_workers$Occ_2digit_clean), "\n")
cat("Ind section categories:", nlevels(df_workers$Ind_section), "\n")

df_workers %>%
  group_by(Gender, Ind_section) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n < 30)

df_workers %>%
  group_by(Gender, Occ_2digit_clean) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n < 30)

cat("Occ 2digit categories:", nlevels(df_workers$Occ_2digit_clean), "\n")
cat("Ind section categories:", nlevels(df_workers$Ind_section), "\n")

# ============================================================
# 9. XUẤT FILE SẠCH
# ============================================================
write_csv(df, "output/clean_data.csv")
write_csv(df_workers, "output/clean_data_workers.csv")

cat("\n=== CHUẨN BỊ DỮ LIỆU ĐÃ XONG! ===\n")

