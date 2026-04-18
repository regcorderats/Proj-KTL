library(car)
# ============================================================
# BƯỚC 3: MÔ HÌNH 2 - PHƯƠNG TRÌNH TIỀN LƯƠNG (OLS VỚI HECKMAN)
# ============================================================

# 0. Loại bỏ 1.337 dòng NA để dữ liệu chạy OLS sạch tuyệt đối
df_wage <- df_workers %>% 
  filter(!is.na(IMR)) %>%
  # Loại bỏ thêm các dòng có NA ở biến độc lập để 3 mô hình đồng nhất số lượng quan sát
  drop_na(ln_Hourly_Wage, Potential_Experience, Highest_Qualification, 
          Marital_Status, Industry_Group, Occupation_Group, Urban, Sector)

cat("Số quan sát đưa vào chạy OLS Tiền lương:", nrow(df_wage), "\n")

# ------------------------------------------------------------
# 1. POOLED OLS (Tìm cấu trúc tiền lương phi phân biệt đối xử \beta^*)
# QUAN TRỌNG: Bắt buộc chứa biến Gender (Dummy) và IMR (\lambda)
# ------------------------------------------------------------
ols_pooled <- lm(
  ln_Hourly_Wage ~ Gender + Potential_Experience + Experience_Squared + 
    Highest_Qualification + Marital_Status + Urban + 
    Industry_Group + Occupation_Group + Sector + 
    IMR,
  data = df_wage
)

cat("\n=== KẾT QUẢ WAGE EQUATION (POOLED) ===\n")
summary(ols_pooled)

# ------------------------------------------------------------
# 2. OLS DÀNH RIÊNG CHO NAM (\beta_M)
# (Không có biến Gender)
# ------------------------------------------------------------
ols_male <- lm(
  ln_Hourly_Wage ~ Potential_Experience + Experience_Squared + 
    Highest_Qualification + Marital_Status + Urban + 
    Industry_Group + Occupation_Group + Sector + 
    IMR,
  data = df_wage %>% filter(Gender == "Nam")
)

cat("\n=== KẾT QUẢ WAGE EQUATION (NAM) ===\n")
summary(ols_male)

# ------------------------------------------------------------
# 3. OLS DÀNH RIÊNG CHO NỮ (\beta_F)
# (Không có biến Gender)
# ------------------------------------------------------------
ols_female <- lm(
  ln_Hourly_Wage ~ Potential_Experience + Experience_Squared + 
    Highest_Qualification + Marital_Status + Urban + 
    Industry_Group + Occupation_Group + Sector + 
    IMR,
  data = df_wage %>% filter(Gender == "Nữ")
)

cat("\n=== KẾT QUẢ WAGE EQUATION (NỮ) ===\n")
summary(ols_female)


# ------------------------------------------------------------
# 4. MÔ HÌNH A: KHÔNG KIỂM SOÁT NGÀNH & NGHỀ
# (Dùng để kiểm định Crowding Hypothesis)
# ------------------------------------------------------------

# Pooled OLS (A)
ols_pooled_A <- lm(
  ln_Hourly_Wage ~ Gender + Potential_Experience + Experience_Squared + 
    Highest_Qualification + Marital_Status + Urban + 
    Sector + # Vẫn giữ Sector (Nhà nước/Tư nhân)
    IMR,     # Vẫn phải có IMR
  data = df_wage
)

# OLS Nam (A)
ols_male_A <- lm(
  ln_Hourly_Wage ~ Potential_Experience + Experience_Squared + 
    Highest_Qualification + Marital_Status + Urban + 
    Sector + IMR,
  data = df_wage %>% filter(Gender == "Nam")
)

# OLS Nữ (A)
ols_female_A <- lm(
  ln_Hourly_Wage ~ Potential_Experience + Experience_Squared + 
    Highest_Qualification + Marital_Status + Urban + 
    Sector + IMR,
  data = df_wage %>% filter(Gender == "Nữ")
)

cat("\n=== KẾT QUẢ WAGE EQUATION - MÔ HÌNH A (NỮ) ===\n")
summary(ols_female_A)