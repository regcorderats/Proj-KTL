# ============================================================
# SCRIPT: Tiền xử lý Dữ liệu LFS 2018
# Nghiên cứu: Decomposing the Gender Wage Gap in Vietnam
#             (Heckman-Oaxaca Approach)
# ============================================================

# ============================================================
# 0. CÀI ĐẶT & NẠP THƯ VIỆN
# ============================================================
library(haven)      
library(dplyr)      
library(ggplot2)    
library(DescTools)  
library(readr)      
library(tidyr)

dir.create("output", showWarnings = FALSE)

# ============================================================
# 1. NẠP FILE DỮ LIỆU GỐC
# ============================================================
df_raw <- read_dta("C:/Users/Wayn/Downloads/LFS_2018 (3)_cut_dup.dta", encoding = "CP1258")

cat("=== DỮ LIỆU GỐC ===\n")
cat("Số quan sát :", nrow(df_raw), "\n")
cat("Số biến     :", ncol(df_raw), "\n")

# ============================================================
# 1.5. TÍNH TOÁN BIẾN CÔNG CỤ (IV) CẤP ĐỘ HỘ GIA ĐÌNH 
# Rất quan trọng: Phải làm trên df_raw trước khi lọc tuổi!
# Yêu cầu file gốc phải có: TINH, HUYEN, XA, THANGDT, HOSO, C2, C3, C5, C21, C22, C23, C24
# ============================================================

df_hh_level <- df_raw %>%
  mutate(
    # 1. Tạo ID hộ duy nhất
    ID_HO_DUY_NHAT = paste(TINH, HUYEN, XA, THANGDT, HOSO, sep = "_"),
    
    # 2A. Biến cho Dependency Ratio
    Trong_Do_Tuoi_LD = case_when(
      C3 == 1 & C5 >= 15 & C5 <= 60 ~ 1,  
      C3 == 2 & C5 >= 15 & C5 <= 55 ~ 1,  
      TRUE ~ 0                              
    ),
    Nguoi_Phu_Thuoc = if_else(Trong_Do_Tuoi_LD == 0, 1, 0),
    
    # 2B. Cờ đánh dấu Trẻ em dưới 6 tuổi
    Is_Child_Under_6 = if_else(C5 < 6, 1, 0),
    
    # 2C. Cờ đánh dấu Ông bà rảnh rỗi
    # Điều kiện 1: Là Bố/mẹ chủ hộ (C2 = 4) HOẶC là người cao tuổi (Nam >=60, Nữ >=55)
    Is_Grandparent = if_else(C2 == 4 | (C3 == 1 & C5 >= 60) | (C3 == 2 & C5 >= 55), 1, 0),
    
    # Điều kiện 2: Không đi làm (C21=2, C22=2, C23=2) VÀ lý do nghỉ KHÔNG PHẢI Tạm nghỉ (C24 != 1)
    # LFS 2018: Câu 21-24 chỉ hỏi người >= 15 tuổi, trẻ em sẽ có NA ở đây.
    Is_Inactive = if_else(C21 == 2 & C22 == 2 & C23 == 2 & (!is.na(C24) & C24 != 1), 1, 0),
    
    Is_Available_GP = if_else(Is_Grandparent == 1 & Is_Inactive == 1, 1, 0)
  ) %>%
  # 3. Gom nhóm theo Hộ gia đình
  group_by(ID_HO_DUY_NHAT) %>%
  summarise(
    Tong_So_Thanh_Vien = n(),                             
    So_Nguoi_Phu_Thuoc = sum(Nguoi_Phu_Thuoc, na.rm = TRUE), 
    
    # Dùng sum() > 0 an toàn hơn max() để tránh sinh ra -Inf khi toàn bộ cột là NA
    Has_Child_Under_6  = if_else(sum(Is_Child_Under_6, na.rm = TRUE) > 0, 1, 0),
    Has_Available_GP   = if_else(sum(Is_Available_GP, na.rm = TRUE) > 0, 1, 0),
    
    .groups = "drop"
  ) %>%
  # 4. Chốt hạ biến công cụ
  mutate(
    HH_Dependency_Ratio  = So_Nguoi_Phu_Thuoc / Tong_So_Thanh_Vien,
    effective_gp_support = if_else(Has_Child_Under_6 == 1 & Has_Available_GP == 1, 1, 0)
  ) %>%
  select(ID_HO_DUY_NHAT, HH_Dependency_Ratio, effective_gp_support)


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
df_male   <- df %>% filter(C3 == 1)
df_female <- df %>% filter(C3 == 2)

cat("\n=== CẤU TRÚC MẪU PROBIT ĐÃ FULL BIẾN ===\n")
cat("  Mẫu Nam (C3=1)  :", nrow(df_male),   "\n")
cat("  Mẫu Nữ (C3=2)   :", nrow(df_female), "\n")
# ============================================================
# 9. XUẤT FILE SẠCH
# ============================================================
write_csv(df, "output/clean_data.csv")
write_csv(df_workers, "output/clean_data_workers.csv")

cat("\n=== CHUẨN BỊ DỮ LIỆU ĐÃ XONG! ===\n")

