# ============================================================
# ROBUSTNESS TEST 3: KIỂM ĐỊNH ĐỘ VỮNG VỚI OUTLIERS (TRIMMING 5% - 95%)
# Mục tiêu: Xem kết quả có bị lèo lái bởi giới siêu giàu / siêu nghèo không
# ============================================================
library(dplyr)

cat("\n--- BƯỚC 1: XÁC ĐỊNH MỐC CẮT (TRIMMING) 5% - 95% ---\n")
# Lấy toàn bộ lương của những người đi làm (cả nam và nữ) để tìm mốc phân vị
all_wages <- c(df_male_boot_ready$ln_Hourly_Wage, df_female_boot_ready$ln_Hourly_Wage)
all_wages <- na.omit(all_wages)

q05 <- quantile(all_wages, 0.05)
q95 <- quantile(all_wages, 0.95)

cat("Mốc 5% (Log Wage):", round(q05, 4), "\n")
cat("Mốc 95% (Log Wage):", round(q95, 4), "\n")

# Lọc dữ liệu: Giữ lại những người ở nhà (is.na) HOẶC những người đi làm có lương nằm trong khoảng 5%-95%
df_M_trim <- df_male_boot_ready %>%
  filter(is.na(ln_Hourly_Wage) | (ln_Hourly_Wage >= q05 & ln_Hourly_Wage <= q95))

df_F_trim <- df_female_boot_ready %>%
  filter(is.na(ln_Hourly_Wage) | (ln_Hourly_Wage >= q05 & ln_Hourly_Wage <= q95))

cat("Số quan sát Nam sau Trimming:", nrow(df_M_trim), "(Gốc:", nrow(df_male_boot_ready), ")\n")
cat("Số quan sát Nữ sau Trimming:", nrow(df_F_trim), "(Gốc:", nrow(df_female_boot_ready), ")\n")


# ============================================================
# BƯỚC 2: VÒNG LẶP BOOTSTRAP CHO TẬP TRIMMING
# ============================================================
set.seed(2026)
B <- 50       # <--- Đang chạy thám thính 50 vòng
vars_needed <- c("ln_Hourly_Wage", "Potential_Experience", "Experience_Squared",
                 "Highest_Qualification", "Marital_Status", "Urban", "Sector",
                 "Ind_section", "Occ_2digit_clean")

boot_results <- matrix(NA, nrow = B, ncol = 4)
colnames(boot_results) <- c("Total_Gap", "Endowments", "Discrimination", "Selection")

cat("\n===========================================================\n")
cat("🔥 ĐANG CHẠY BOOTSTRAP CHO TẬP DỮ LIỆU ĐÃ TRIMMING (5%-95%) 🔥\n")
cat("===========================================================\n")

for (i in 1:B) {
  if (i %% 10 == 0) cat("  Tiến độ Bootstrap:", i, "/", B, "\n")
  
  tryCatch({
    # Resample trên tập Trimmed
    boot_M_full <- df_M_trim[sample(nrow(df_M_trim), replace = TRUE), ]
    boot_F_full <- df_F_trim[sample(nrow(df_F_trim), replace = TRUE), ]
    
    # Probit
    probit_F_boot <- glm(
      LFP ~ Potential_Experience + Experience_Squared + Highest_Qualification + 
        Urban + Marital_Status + as.factor(TINH) + Has_Available_GP,
      family = binomial(link = "probit"), 
      data = boot_F_full,
      na.action = na.exclude
    )
    
    Z_gamma_boot <- predict(probit_F_boot, type = "link")
    boot_F_full$IMR_new <- dnorm(Z_gamma_boot) / pnorm(Z_gamma_boot)
    
    # Lọc OLS
    boot_wage_M <- boot_M_full[boot_M_full$LFP == 1, ]
    boot_wage_F <- boot_F_full[boot_F_full$LFP == 1, ]
    
    boot_wage_M <- na.omit(boot_wage_M[, vars_needed])
    boot_wage_F <- na.omit(boot_wage_F[, c(vars_needed, "IMR_new")])
    
    # Chạy OLS
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
    
    # Trích xuất ma trận
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
    
    # Phân rã
    Endowments_boot <- sum((X_bar_M_boot - X_bar_F_boot) * beta_M_boot)
    Discrimination_boot <- sum(X_bar_F_boot * (beta_M_boot - beta_F_boot))
    Selection_boot <- - (theta_F_boot * lambda_bar_F_boot)
    Total_Gap_boot <- mean(boot_wage_M$ln_Hourly_Wage) - mean(boot_wage_F$ln_Hourly_Wage)
    
    boot_results[i, ] <- c(Total_Gap_boot, Endowments_boot, Discrimination_boot, Selection_boot)
    
  }, error = function(e) {})
}

# ==========================================================
# BƯỚC 3: IN KẾT QUẢ TỔNG HỢP
# ==========================================================
boot_results_clean <- na.omit(boot_results)
cat("\n>> Hoàn thành! Số vòng thành công:", nrow(boot_results_clean), "/", B, "\n")

if(nrow(boot_results_clean) > 0) {
  boot_se <- apply(boot_results_clean, 2, sd)
  boot_ci_lower <- apply(boot_results_clean, 2, function(x) quantile(x, probs = 0.025))
  boot_ci_upper <- apply(boot_results_clean, 2, function(x) quantile(x, probs = 0.975))
  point_estimates <- apply(boot_results_clean, 2, mean)
  
  Trim_Result_DF <- data.frame(
    Component = c("1. Total Observed Wage Gap", 
                  "2. Endowments (Explained)", 
                  "3. Discrimination (Unexplained)", 
                  "4. Selection Effect"),
    Value = point_estimates,
    Std_Error = boot_se,
    CI_95_Lower = boot_ci_lower,
    CI_95_Upper = boot_ci_upper
  )
  
  Trim_Result_DF$Significant <- ifelse(
    (Trim_Result_DF$CI_95_Lower > 0 & Trim_Result_DF$CI_95_Upper > 0) | 
      (Trim_Result_DF$CI_95_Lower < 0 & Trim_Result_DF$CI_95_Upper < 0), 
    "Yes (***)", "No"
  )
  
  cat("\n###########################################################\n")
  cat("   KẾT QUẢ OAXACA SAU KHI LOẠI BỎ NGOẠI LAI (TRIMMING 5-95%) \n")
  cat("###########################################################\n")
  print(Trim_Result_DF[, c("Component", "Value", "CI_95_Lower", "CI_95_Upper", "Significant")], row.names = FALSE)
}