library(car)
# ============================================================
# BƯỚC 3: MÔ HÌNH 2 - PHƯƠNG TRÌNH TIỀN LƯƠNG (OLS VỚI HECKMAN)
# ============================================================

# 0. Lọc NA & Khóa cứng cỡ mẫu cho tất cả các mô hình
df_wage <- df_workers %>%
  mutate(
    IMR_Female = ifelse(Gender == "Nữ", IMR_new, 0)  # Đảm bảo dùng IMR_new từ Probit Single IV
  ) %>%
  drop_na(
    # Biến cơ bản & Heckman
    ln_Hourly_Wage, Potential_Experience, Highest_Qualification,
    Marital_Status, Urban, Sector, IMR_Female,
    
    # Biến địa lý (Bắt buộc phải có TINH để chạy Falsification không bị rớt mẫu)
    TINH, VUNG,
    
    # Biến kiểm soát Model B (Ngành/Nghề cơ bản)
    Industry_Group, Occupation_Group,
    
    # Biến kiểm soát Model B_fine (Ngành/Nghề chi tiết)
    Ind_section, Occ_2digit_clean
  )

cat("Số quan sát OLS (Đã khóa mẫu):", nrow(df_wage), "\n")


# ── MÔ HÌNH A (Cơ bản - Không Ngành/Nghề) ──
ols_male_A <- lm(
  ln_Hourly_Wage ~ Potential_Experience + Experience_Squared + 
    Highest_Qualification + Marital_Status + Urban + Sector,
  data = df_wage %>% filter(Gender == "Nam")
)
ols_female_A <- lm(
  ln_Hourly_Wage ~ Potential_Experience + Experience_Squared + 
    Highest_Qualification + Marital_Status + Urban + Sector + IMR_Female,
  data = df_wage %>% filter(Gender == "Nữ")
)

# ── MÔ HÌNH B (Kiểm soát Ngành/Nghề 1-digit) ──
ols_male <- lm(
  ln_Hourly_Wage ~ Potential_Experience + Experience_Squared +
    Highest_Qualification + Marital_Status + Urban + Sector +
    Industry_Group + Occupation_Group,
  data = df_wage %>% filter(Gender == "Nam")
)
ols_female <- lm(
  ln_Hourly_Wage ~ Potential_Experience + Experience_Squared +
    Highest_Qualification + Marital_Status + Urban + Sector +
    Industry_Group + Occupation_Group + IMR_Female,
  data = df_wage %>% filter(Gender == "Nữ")
)

# ── MÔ HÌNH B_FINE (Kiểm soát Ngành/Nghề 2-digit) ──
ols_male_Bfine <- lm(
  ln_Hourly_Wage ~ Potential_Experience + Experience_Squared +
    Highest_Qualification + Marital_Status + Urban + Sector +
    Ind_section + Occ_2digit_clean,
  data = df_wage %>% filter(Gender == "Nam")
)
ols_female_Bfine <- lm(
  ln_Hourly_Wage ~ Potential_Experience + Experience_Squared +
    Highest_Qualification + Marital_Status + Urban + Sector +
    Ind_section + Occ_2digit_clean + IMR_Female,
  data = df_wage %>% filter(Gender == "Nữ")
)

# ── BÀI TEST TÍNH NGOẠI SINH CỦA IV (FALSIFICATION TEST) ──
cat("\n=== MÔ HÌNH OLS TEST TÍNH NGOẠI SINH CỦA IV (NỮ) ===\n")
ols_falsification_female <- lm(
  ln_Hourly_Wage ~ Potential_Experience + Experience_Squared +
    Highest_Qualification + Marital_Status + Urban + Sector +
    Ind_section + Occ_2digit_clean + as.factor(TINH) + Has_Available_GP,
  data = df_wage %>% filter(Gender == "Nữ")
)

falsification_results <- summary(ols_falsification_female)$coefficients
cat("\n--- P-VALUE CỦA BIẾN IV TRONG PHƯƠNG TRÌNH LƯƠNG ---\n")
print(falsification_results["Has_Available_GP", , drop=FALSE])