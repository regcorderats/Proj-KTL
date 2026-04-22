# ============================================================
# BƯỚC 5: BOOTSTRAP OAXACA DECOMPOSITION (ĐÃ VÁ LỖI THIẾU CỘT)
# ============================================================
library(dplyr)

cat("\n--- BƯỚC 0: ĐỒNG BỘ DỮ LIỆU TRƯỚC KHI BOOTSTRAP ---\n")
# Trích xuất các biến Lương/Ngành/Nghề từ df_workers để ghép ngược lại
cols_to_join <- df_workers %>%
  select(ID_CA_NHAN, ln_Hourly_Wage, Sector, Ind_section, Occ_2digit_clean)

# Ghép vào tập gốc
df_male_boot_ready <- df_male %>% left_join(cols_to_join, by = "ID_CA_NHAN")
df_female_boot_ready <- df_female %>% left_join(cols_to_join, by = "ID_CA_NHAN")

cat("Đã cập nhật đủ cột cho df_male_boot_ready và df_female_boot_ready!\n")


# ============================================================
# VÒNG LẶP BOOTSTRAP
# ============================================================
set.seed(2026)
B <- 500       

boot_results <- matrix(NA, nrow = B, ncol = 4)
colnames(boot_results) <- c("Total_Gap", "Endowments", "Discrimination", "Selection")

# Xác định trước các biến cần thiết để subset bằng Base R
vars_needed <- c("ln_Hourly_Wage", "Potential_Experience", "Experience_Squared",
                 "Highest_Qualification", "Marital_Status", "Urban", "Sector",
                 "Ind_section", "Occ_2digit_clean")

cat("\nKhởi động Bootstrap", B, "vòng...\n")

for (i in 1:B) {
  if (i %% 50 == 0) cat("Đang chạy vòng thứ:", i, "/", B, "\n")
  
  tryCatch({
    # 1. Resample từ tập dữ liệu ĐÃ VÁ LỖI
    boot_M_full <- df_male_boot_ready[sample(nrow(df_male_boot_ready), replace = TRUE), ]
    boot_F_full <- df_female_boot_ready[sample(nrow(df_female_boot_ready), replace = TRUE), ]
    
    # 2. Re-estimate Selection Equation cho Nữ
    probit_F_boot <- glm(
      LFP ~ Potential_Experience + Experience_Squared + Highest_Qualification + 
        Urban + Marital_Status + as.factor(TINH) + Has_Available_GP,
      family = binomial(link = "probit"), 
      data = boot_F_full,
      na.action = na.exclude
    )
    
    Z_gamma_boot <- predict(probit_F_boot, type = "link")
    boot_F_full$IMR_new <- dnorm(Z_gamma_boot) / pnorm(Z_gamma_boot)
    
    # 3. Lọc người đi làm (LFP = 1)
    boot_wage_M <- boot_M_full[boot_M_full$LFP == 1, ]
    boot_wage_F <- boot_F_full[boot_F_full$LFP == 1, ]
    
    # Lọc bỏ NA trên các cột cần thiết cho OLS
    boot_wage_M <- na.omit(boot_wage_M[, vars_needed])
    boot_wage_F <- na.omit(boot_wage_F[, c(vars_needed, "IMR_new")])
    
    # 4. Chạy OLS
    ols_M_boot <- lm(
      ln_Hourly_Wage ~ Potential_Experience + Experience_Squared +
        Highest_Qualification + Marital_Status + Urban + Sector +
        Ind_section + Occ_2digit_clean,
      data = boot_wage_M
    )
    
    ols_F_boot <- lm(
      ln_Hourly_Wage ~ Potential_Experience + Experience_Squared +
        Highest_Qualification + Marital_Status + Urban + Sector +
        Ind_section + Occ_2digit_clean + IMR_new,
      data = boot_wage_F
    )
    
    # 5. Đồng bộ ma trận và tính toán Oaxaca
    beta_M_boot <- coef(ols_M_boot)
    beta_F_full_boot <- coef(ols_F_boot)
    theta_F_boot <- beta_F_full_boot["IMR_new"]
    
    male_vars_boot <- names(beta_M_boot)
    beta_F_boot <- beta_F_full_boot[male_vars_boot]
    beta_F_boot[is.na(beta_F_boot)] <- 0
    names(beta_F_boot) <- male_vars_boot
    
    X_bar_M_boot <- colMeans(model.matrix(ols_M_boot))
    X_bar_F_full_boot <- colMeans(model.matrix(ols_F_boot))
    lambda_bar_F_boot <- X_bar_F_full_boot["IMR_new"]
    
    X_bar_F_boot <- X_bar_F_full_boot[male_vars_boot]
    X_bar_F_boot[is.na(X_bar_F_boot)] <- 0
    names(X_bar_F_boot) <- male_vars_boot
    
    # 6. Lắp công thức
    Endowments_boot <- sum((X_bar_M_boot - X_bar_F_boot) * beta_M_boot)
    Discrimination_boot <- sum(X_bar_F_boot * (beta_M_boot - beta_F_boot))
    Selection_boot <- - (theta_F_boot * lambda_bar_F_boot)
    Total_Gap_boot <- mean(boot_wage_M$ln_Hourly_Wage) - mean(boot_wage_F$ln_Hourly_Wage)
    
    boot_results[i, ] <- c(Total_Gap_boot, Endowments_boot, Discrimination_boot, Selection_boot)
    
  }, error = function(e) {
    cat("\n[!] Lỗi ở vòng", i, ":", conditionMessage(e), "\n")
  })
}

# ============================================================
# TÍNH TOÁN KHOẢNG TIN CẬY BOOTSTRAP (PERCENTILE METHOD)
# ============================================================
boot_results_clean <- na.omit(boot_results)
cat("\nSố vòng Bootstrap thành công:", nrow(boot_results_clean), "/", B, "\n")

if(nrow(boot_results_clean) > 0) {
  boot_se <- apply(boot_results_clean, 2, sd)
  boot_ci_lower <- apply(boot_results_clean, 2, function(x) quantile(x, probs = 0.025))
  boot_ci_upper <- apply(boot_results_clean, 2, function(x) quantile(x, probs = 0.975))
  
  Oaxaca_Final$Std_Error <- boot_se
  Oaxaca_Final$CI_95_Lower <- boot_ci_lower
  Oaxaca_Final$CI_95_Upper <- boot_ci_upper
  
  Oaxaca_Final$Significant <- ifelse(
    (Oaxaca_Final$CI_95_Lower > 0 & Oaxaca_Final$CI_95_Upper > 0) | 
      (Oaxaca_Final$CI_95_Lower < 0 & Oaxaca_Final$CI_95_Upper < 0), 
    "Yes (***)", "No"
  )
  
  cat("\n===========================================================\n")
  cat("   BẢNG OAXACA CUỐI CÙNG (BOOTSTRAP 95% CONFIDENCE INTERVAL)\n")
  cat("===========================================================\n")
  print(Oaxaca_Final[, c("Component", "Value", "Std_Error", "CI_95_Lower", "CI_95_Upper", "Significant")], row.names = FALSE)
} else {
  cat("\n[!] Tất cả các vòng lặp đều thất bại. Hãy kiểm tra lại log lỗi.\n")
}