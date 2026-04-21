# ============================================================
# BƯỚC 4: PHÂN RÃ OAXACA CHUẨN NAM (MALE-REFERENCE) CHO MODEL B_FINE
# Mục tiêu: Bảng kết quả cuối cùng nộp cho hội đồng
# ============================================================

# 1. TRÍCH XUẤT HỆ SỐ TỪ MÔ HÌNH (Dùng OLS Nam làm gốc)
beta_M      <- coef(ols_male_Bfine)
beta_F_full <- coef(ols_female_Bfine)

# Trích xuất hệ số IMR của Nữ
theta_F <- beta_F_full["IMR_Female"]

# 2. ĐỒNG BỘ HÓA MA TRẬN (BỌC LÓT LỖI RỚT BIẾN DUMMY 2-DIGIT)
# Lấy danh sách tên biến của Nam làm gốc (Male-reference)
male_vars <- names(beta_M)

# Đồng bộ beta_F theo Nam: Nếu Nữ không có ngành/nghề đó (NA), ép về 0
beta_F <- beta_F_full[male_vars]
beta_F[is.na(beta_F)] <- 0
names(beta_F) <- male_vars

# Trích xuất giá trị trung bình (Mean Endowments)
X_mat_M <- model.matrix(ols_male_Bfine)
X_bar_M <- colMeans(X_mat_M)

X_mat_F_full <- model.matrix(ols_female_Bfine)
X_bar_F_full <- colMeans(X_mat_F_full)

# Trích xuất riêng IMR trung bình của Nữ
lambda_bar_F <- X_bar_F_full["IMR_Female"]

# Đồng bộ X_bar_F theo Nam: Nếu Nữ không có ngành/nghề đó (NA), ép về 0
X_bar_F <- X_bar_F_full[male_vars]
X_bar_F[is.na(X_bar_F)] <- 0
names(X_bar_F) <- male_vars


# 3. TÍNH TOÁN CÁC CẤU PHẦN PHÂN RÃ (OAXACA FORMULAS)
# Endowments (Phần giải thích được): Chênh lệch vốn nhân lực đánh giá theo giá của Nam
Endowments <- sum((X_bar_M - X_bar_F) * beta_M)

# Discrimination (Phân biệt đối xử): Chênh lệch giá cả/hệ số trên nền vốn nhân lực của Nữ
Discrimination <- sum(X_bar_F * (beta_M - beta_F))

# Selection (Thiên lệch chọn mẫu): Tác động của rào cản gia đình
Selection_F <- - (theta_F * lambda_bar_F)


# 4. TÍNH TỔNG GAP VÀ KIỂM TRA SAI SỐ THUẬT TOÁN
W_M_bar <- mean(ols_male_Bfine$model$ln_Hourly_Wage)
W_F_bar <- mean(ols_female_Bfine$model$ln_Hourly_Wage)
Total_Gap <- W_M_bar - W_F_bar

Sum_Components <- Endowments + Discrimination + Selection_F
Tolerance <- Total_Gap - Sum_Components


# 5. IN BẢNG KẾT QUẢ CUỐI CÙNG
Oaxaca_Final <- data.frame(
  Component = c(
    "1. Total Observed Wage Gap",
    "2. Endowments (Explained by Characteristics)",
    "3. Discrimination (Unexplained / Coefficients)",
    "4. Selection Effect (Family Constraints)"
  ),
  Value = c(
    Total_Gap,
    Endowments,
    Discrimination,
    Selection_F
  ),
  Percentage = c(
    100.0,
    (Endowments / Total_Gap) * 100,
    (Discrimination / Total_Gap) * 100,
    (Selection_F / Total_Gap) * 100
  )
)

cat("\n===========================================================\n")
cat("   KẾT QUẢ PHÂN RÃ OAXACA CHUẨN NAM (MALE-REFERENCE)       \n")
cat("===========================================================\n")
print(Oaxaca_Final, row.names = FALSE)
cat("\n-----------------------------------------------------------\n")
cat("Sai số phân rã (Tolerance):", format(Tolerance, scientific = TRUE), "\n")
cat("Kỳ vọng: < 1e-10 (Nếu đạt, cấu trúc toán học hoàn hảo!)\n")