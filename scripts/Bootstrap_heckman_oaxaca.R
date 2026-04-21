# ============================================================
# BƯỚC 5: BOOTSTRAP STANDARD ERRORS CHO OAXACA DECOMPOSITION
# ============================================================
library(dplyr)
library(tidyr)

# 1. CHUẨN BỊ MẪU GỐC (Dùng lại df_male và df_female từ Bước 1)
# Đảm bảo df_male và df_female đã có đầy đủ các biến tạo ra từ create_model_vars()
# và các biến 2-digit.

set.seed(2026) # Khóa seed để kết quả có thể tái lập
B <- 500       # Số vòng lặp (Hội đồng thường yêu cầu 500 - 1000)

# Khởi tạo ma trận lưu kết quả
boot_results <- matrix(NA, nrow = B, ncol = 4)
colnames(boot_results) <- c("Total_Gap", "Endowments", "Discrimination", "Selection")

cat("\nKhởi động Bootstrap", B, "vòng. Sẽ mất một chút thời gian...\n")

# 2. VÒNG LẶP BOOTSTRAP
for (i in 1:B) {
  
  # In tiến độ mỗi 50 vòng để theo dõi
  if (i %% 50 == 0) cat("Đang chạy vòng thứ:", i, "/", B, "\n")
  
  tryCatch({
    # ----------------------------------------------------
    # Bước 2.1: Lấy mẫu ngẫu nhiên có hoàn lại (Stratified)
    # ----------------------------------------------------
    idx_M <- sample(1:nrow(df_male), nrow(df_male), replace = TRUE)
    idx_F <- sample(1:nrow(df_female), nrow(df_female), replace = TRUE)
    
    boot_M_full <- df_male[idx_M, ]
    boot_F_full <- df_female[idx_F, ]
    
    # ----------------------------------------------------
    # Bước 2.2: Chạy lại Probit và tính IMR MỚI trên tập Boot Nữ
    # ----------------------------------------------------
    probit_F_boot <- glm(
      LFP ~ Potential_Experience + Experience_Squared + Highest_Qualification + 
        Urban + Marital_Status + as.factor(TINH) + Has_Available_GP,
      family = binomial(link = "probit"), 
      data = boot_F_full,
      na.action = na.exclude
    )
    
    Z_gamma_boot <- predict(probit_F_boot, type = "link")
    boot_F_full$IMR_new <- dnorm(Z_gamma_boot) / pnorm(Z_gamma_boot)
    
    # ----------------------------------------------------
    # Bước 2.3: Lọc dữ liệu Wage (LFP == 1) và drop NA cho OLS
    # ----------------------------------------------------
    vars_to_drop_na <- c("ln_Hourly_Wage", "Potential_Experience", "Highest_Qualification",
                         "Marital_Status", "Urban", "Sector", "TINH", "VUNG",
                         "Ind_section", "Occ_2digit_clean")
    
    boot_wage_M <- boot_M_full %>% 
      filter(LFP == 1) %>% 
      drop_na(all_of(vars_to_drop_na))
    
    boot_wage_F <- boot_F_full %>% 
      filter(LFP == 1) %>% 
      drop_na(all_of(c(vars_to_drop_na, "IMR_new")))
    
    # ----------------------------------------------------
    # Bước 2.4: Chạy OLS (Model B_fine)
    # ----------------------------------------------------
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
    
    # ----------------------------------------------------
    # Bước 2.5: Trích xuất và Đồng bộ Hệ số cho Oaxaca
    # ----------------------------------------------------
    beta_M_boot <- coef(ols_M_boot)
    beta_F_full_boot <- coef(ols_F_boot)
    
    theta_F_boot <- beta_F_full_boot["IMR_new"]
    
    # Đồng bộ beta_F theo Nam
    male_vars_boot <- names(beta_M_boot)
    beta_F_boot <- beta_F_full_boot[male_vars_boot]
    beta_F_boot[is.na(beta_F_boot)] <- 0
    names(beta_F_boot) <- male_vars_boot
    
    # Đồng bộ X_bar
    X_bar_M_boot <- colMeans(model.matrix(ols_M_boot))
    X_bar_F_full_boot <- colMeans(model.matrix(ols_F_boot))
    
    lambda_bar_F_boot <- X_bar_F_full_boot["IMR_new"]
    
    X_bar_F_boot <- X_bar_F_full_boot[male_vars_boot]
    X_bar_F_boot[is.na(X_bar_F_boot)] <- 0
    names(X_bar_F_boot) <- male_vars_boot
    
    # ----------------------------------------------------
    # Bước 2.6: Tính toán cấu phần Oaxaca và Lưu kết quả
    # ----------------------------------------------------
    Endowments_boot <- sum((X_bar_M_boot - X_bar_F_boot) * beta_M_boot)
    Discrimination_boot <- sum(X_bar_F_boot * (beta_M_boot - beta_F_boot))
    Selection_boot <- - (theta_F_boot * lambda_bar_F_boot)
    Total_Gap_boot <- mean(boot_wage_M$ln_Hourly_Wage) - mean(boot_wage_F$ln_Hourly_Wage)
    
    boot_results[i, ] <- c(Total_Gap_boot, Endowments_boot, Discrimination_boot, Selection_boot)
    
  }, error = function(e) {
    # Nếu vòng lặp bị lỗi do rớt level của dummy, hệ thống sẽ im lặng chuyển sang vòng sau
    # Dòng này sẽ trả về NA, ta sẽ loại bỏ NA ở bước tính toán cuối cùng
  })
}

# 3. XỬ LÝ KẾT QUẢ BOOTSTRAP VÀ TÍNH STANDARD ERROR
# Loại bỏ các vòng lặp bị lỗi (NA)
boot_results_clean <- na.omit(boot_results)
successful_loops <- nrow(boot_results_clean)

cat("\nSố vòng Bootstrap thành công:", successful_loops, "/", B, "\n")

# Tính Standard Deviation (Chính là Standard Error của hệ số)
boot_se <- apply(boot_results_clean, 2, sd)

# Gắn SE vào bảng kết quả gốc của bạn
# (Giả sử bạn đã có dataframe Oaxaca_Final từ script trước)
Oaxaca_Final$Std_Error <- boot_se
Oaxaca_Final$t_value <- Oaxaca_Final$Value / Oaxaca_Final$Std_Error
Oaxaca_Final$p_value <- 2 * pt(-abs(Oaxaca_Final$t_value), df = nrow(df_wage) - length(coef(ols_male_Bfine)))

cat("\n===========================================================\n")
cat("   BẢNG OAXACA CUỐI CÙNG (VỚI BOOTSTRAP STANDARD ERRORS)   \n")
cat("===========================================================\n")
print(Oaxaca_Final, row.names = FALSE)