# ============================================================
# ROBUSTNESS TEST 2: KIỂM ĐỊNH CƠ CHẾ QUA ĐỘ TUỔI (MOTHERHOOD PENALTY)
# Mục tiêu: So sánh Selection Effect giữa nhóm 25-35 tuổi và 36-55 tuổi
# ============================================================
library(dplyr)

cat("\n--- BƯỚC 1: CHIA LƯỚI ĐỘ TUỔI ---\n")
# Tạo biến Age_Cohort cho cả 2 tập dữ liệu đã sẵn sàng Bootstrap
df_male_boot_ready <- df_male_boot_ready %>%
  mutate(Age_Cohort = if_else(C5 >= 25 & C5 <= 35, "Age_25_35", "Age_36_55"))

df_female_boot_ready <- df_female_boot_ready %>%
  mutate(Age_Cohort = if_else(C5 >= 25 & C5 <= 35, "Age_25_35", "Age_36_55"))


# ============================================================
# BƯỚC 2: KHAI BÁO THÔNG SỐ CHUNG
# ============================================================
set.seed(2026)
B <- 50       # <--- ĐANG ĐỂ 50 CHO "SCOUT RUN". NẾU SỐ ĐẸP, ĐỔI THÀNH 200 ĐỂ CHỐT SỔ.
cohorts_to_run <- c("Age_25_35", "Age_36_55")

Final_Age_Results <- list()

vars_needed <- c("ln_Hourly_Wage", "Potential_Experience", "Experience_Squared",
                 "Highest_Qualification", "Marital_Status", "Urban", "Sector",
                 "Ind_section", "Occ_2digit_clean")

# ============================================================
# BƯỚC 3: VÒNG LẶP LỚN (OUTER LOOP) - CHẠY TỪNG NHÓM TUỔI
# ============================================================
for (current_cohort in cohorts_to_run) {
  
  cat("\n===========================================================\n")
  cat("🔥 ĐANG XỬ LÝ NHÓM TUỔI:", current_cohort, "🔥\n")
  cat("===========================================================\n")
  
  # Cắt mẫu theo Độ tuổi hiện tại
  df_M_sub <- df_male_boot_ready %>% filter(Age_Cohort == current_cohort)
  df_F_sub <- df_female_boot_ready %>% filter(Age_Cohort == current_cohort)
  
  boot_results <- matrix(NA, nrow = B, ncol = 4)
  colnames(boot_results) <- c("Total_Gap", "Endowments", "Discrimination", "Selection")
  
  # ==========================================================
  # VÒNG LẶP BOOTSTRAP (INNER LOOP)
  # ==========================================================
  for (i in 1:B) {
    if (i %% 10 == 0) cat("  Tiến độ Bootstrap:", i, "/", B, "\n") # Báo cáo mỗi 10 vòng
    
    tryCatch({
      # Resample
      boot_M_full <- df_M_sub[sample(nrow(df_M_sub), replace = TRUE), ]
      boot_F_full <- df_F_sub[sample(nrow(df_F_sub), replace = TRUE), ]
      
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
      
    }, error = function(e) {
      # Bỏ qua lỗi
    })
  }
  
  # ==========================================================
  # BƯỚC 4: TỔNG HỢP KẾT QUẢ CHO TỪNG NHÓM
  # ==========================================================
  boot_results_clean <- na.omit(boot_results)
  cat("\n  >> Hoàn thành", current_cohort, "- Số vòng thành công:", nrow(boot_results_clean), "/", B, "\n")
  
  if(nrow(boot_results_clean) > 0) {
    boot_se <- apply(boot_results_clean, 2, sd)
    boot_ci_lower <- apply(boot_results_clean, 2, function(x) quantile(x, probs = 0.025))
    boot_ci_upper <- apply(boot_results_clean, 2, function(x) quantile(x, probs = 0.975))
    point_estimates <- apply(boot_results_clean, 2, mean)
    
    Cohort_Result_DF <- data.frame(
      Component = c("1. Total Observed Wage Gap", 
                    "2. Endowments (Explained)", 
                    "3. Discrimination (Unexplained)", 
                    "4. Selection Effect"),
      Value = point_estimates,
      Std_Error = boot_se,
      CI_95_Lower = boot_ci_lower,
      CI_95_Upper = boot_ci_upper
    )
    
    Cohort_Result_DF$Significant <- ifelse(
      (Cohort_Result_DF$CI_95_Lower > 0 & Cohort_Result_DF$CI_95_Upper > 0) | 
        (Cohort_Result_DF$CI_95_Lower < 0 & Cohort_Result_DF$CI_95_Upper < 0), 
      "Yes (***)", "No"
    )
    
    Final_Age_Results[[current_cohort]] <- Cohort_Result_DF
  }
}

# ============================================================
# BƯỚC 5: IN TOÀN BỘ KẾT QUẢ RA MÀN HÌNH
# ============================================================
cat("\n\n###########################################################\n")
cat("   KIỂM ĐỊNH MOTHERHOOD PENALTY THEO NHÓM TUỔI (BOOTSTRAP)\n")
cat("###########################################################\n")

for (cohort in names(Final_Age_Results)) {
  cat("\n---> ĐỘ TUỔI:", cohort, "<---\n")
  print(Final_Age_Results[[cohort]][, c("Component", "Value", "CI_95_Lower", "CI_95_Upper", "Significant")], row.names = FALSE)
  cat("-----------------------------------------------------------\n")
}