# ============================================================
# ROBUSTNESS TEST 1: NAIVE OAXACA (KHÔNG HECKMAN)
# Mục tiêu: Chứng minh mô hình OLS truyền thống đánh giá sai mức độ phân biệt đối xử
# ============================================================

# 1. CHẠY LẠI OLS "MÙ" (Mô hình B_fine nhưng VỨT BỎ biến IMR ở nhóm Nữ)
ols_male_naive <- lm(
  ln_Hourly_Wage ~ Potential_Experience + Experience_Squared +
    Highest_Qualification + Marital_Status + Urban + Sector +
    Ind_section + Occ_2digit_clean,
  data = df_wage %>% filter(Gender == "Nam")
)

ols_female_naive <- lm(
  ln_Hourly_Wage ~ Potential_Experience + Experience_Squared +
    Highest_Qualification + Marital_Status + Urban + Sector +
    Ind_section + Occ_2digit_clean,   # <--- HOÀN TOÀN KHÔNG CÓ IMR_FEMALE
  data = df_wage %>% filter(Gender == "Nữ")
)

# 2. TRÍCH XUẤT VÀ ĐỒNG BỘ MA TRẬN (MALE-REFERENCE)
beta_M_naive <- coef(ols_male_naive)
beta_F_full_naive <- coef(ols_female_naive)

male_vars_naive <- names(beta_M_naive)
beta_F_naive <- beta_F_full_naive[male_vars_naive]
beta_F_naive[is.na(beta_F_naive)] <- 0
names(beta_F_naive) <- male_vars_naive

X_bar_M_naive <- colMeans(model.matrix(ols_male_naive))
X_bar_F_full_naive <- colMeans(model.matrix(ols_female_naive))

X_bar_F_naive <- X_bar_F_full_naive[male_vars_naive]
X_bar_F_naive[is.na(X_bar_F_naive)] <- 0
names(X_bar_F_naive) <- male_vars_naive


# 3. TÍNH TOÁN OAXACA 2 CẤU PHẦN (Lúc này Selection = 0)
Endowments_naive <- sum((X_bar_M_naive - X_bar_F_naive) * beta_M_naive)
Discrimination_naive <- sum(X_bar_F_naive * (beta_M_naive - beta_F_naive))

Total_Gap_naive <- mean(ols_male_naive$model$ln_Hourly_Wage) - mean(ols_female_naive$model$ln_Hourly_Wage)
Tolerance_naive <- Total_Gap_naive - (Endowments_naive + Discrimination_naive)


# 4. IN BẢNG KẾT QUẢ SO SÁNH
Naive_Results <- data.frame(
  Component = c(
    "1. Total Observed Wage Gap",
    "2. Endowments (Explained)",
    "3. Discrimination (Unexplained)"
  ),
  Value = c(
    Total_Gap_naive, 
    Endowments_naive, 
    Discrimination_naive
  ),
  Percentage = c(
    100.0,
    (Endowments_naive / Total_Gap_naive) * 100,
    (Discrimination_naive / Total_Gap_naive) * 100
  )
)

cat("\n===========================================================\n")
cat("   KẾT QUẢ NAIVE OAXACA (KHÔNG HIỆU CHỈNH CHỌN MẪU) \n")
cat("===========================================================\n")
print(Naive_Results, row.names = FALSE)
cat("\nSai số toán học:", format(Tolerance_naive, scientific = TRUE), "\n")