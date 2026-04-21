# ============================================================
# BƯỚC 2: SELECTION EQUATION (PROBIT) & INVERSE MILLS RATIO
# ============================================================
library(car)

# ------------------------------------------------------------
# 1. Chạy mô hình Probit cho NỮ (Single IV - Just Identified)
# ------------------------------------------------------------
probit_female <- glm(
  LFP ~ Potential_Experience + Experience_Squared + Highest_Qualification + 
    Urban + Marital_Status + as.factor(TINH) + 
    Has_Available_GP,    # <-- CHỈ SỬ DỤNG DUY NHẤT 1 IV NÀY
  family = binomial(link = "probit"), 
  data = df_female,
  na.action = na.exclude
)

cat("=== KẾT QUẢ PROBIT MÔ HÌNH NỮ (SINGLE IV & PROVINCE FE) ===\n")
summary(probit_female)

# ------------------------------------------------------------
# 2. Kiểm định sức mạnh biến công cụ (NỮ)
# ------------------------------------------------------------
cat("\n=== WALD TEST: KIỂM ĐỊNH SỨC MẠNH BIẾN CÔNG CỤ (NỮ) ===\n")
# Với 1 IV, ta kiểm định hệ số của Has_Available_GP khác 0
iv_test_female <- linearHypothesis(
  probit_female, 
  "Has_Available_GP = 0"
)
print(iv_test_female)
# ============================================================
# BƯỚC 2.3: TÍNH TOÁN IMR MỚI & CẬP NHẬT DỮ LIỆU
# ============================================================

# 1. Tính toán Z_gamma (linear predictor) và IMR mới cho Nữ
# Lưu ý: predict với probit_female đã có na.exclude nên sẽ trả về vector khớp với df_female
Z_gamma_female <- predict(probit_female, type = "link")
df_female$IMR_new <- dnorm(Z_gamma_female) / pnorm(Z_gamma_female)

# 2. Dọn dẹp: Loại bỏ hoàn toàn các cột liên quan đến IMR cũ trong df_workers
df_workers <- df_workers %>% select(-matches("IMR"))

# 3. Nối (merge) IMR mới từ df_female vào df_workers thông qua ID_CA_NHAN
df_workers <- df_workers %>%
  left_join(
    df_female %>% select(ID_CA_NHAN, IMR_new),
    by = "ID_CA_NHAN"
  )

# 4. Gán tên chuẩn IMR_Female và xử lý giá trị 0 cho Nam
# Điều này đảm bảo khi chạy OLS Pooled, biến IMR_Female không gây rớt quan sát Nam
df_workers <- df_workers %>%
  mutate(
    IMR_Female = ifelse(Gender == "Nữ", IMR_new, 0)
  ) %>%
  # Thay thế các giá trị NA còn sót lại (nếu có) bằng 0 để an toàn cho OLS
  mutate(IMR_Female = replace_na(IMR_Female, 0))

# 5. Kiểm tra phân phối IMR mới
cat("\n=== KIỂM TRA IMR MỚI (Single IV) ===\n")
cat("Số lượng quan sát NA trong IMR_Female:", sum(is.na(df_workers$IMR_Female)), "\n")
cat("Giá trị trung bình IMR (nhóm Nữ):", 
    round(mean(df_workers$IMR_Female[df_workers$Gender == "Nữ"], na.rm = TRUE), 4), "\n")
cat("Giá trị trung bình IMR (nhóm Nam - Kỳ vọng = 0):", 
    round(mean(df_workers$IMR_Female[df_workers$Gender == "Nam"]), 4), "\n")

# Biểu đồ kiểm tra nhanh độ phân tán của IMR
hist(df_workers$IMR_Female[df_workers$Gender == "Nữ"], 
     main="Phân phối IMR mới (Nữ)", col="lightblue", breaks=50)

# ============================================================
# BƯỚC 3: VERIFICATION (TEST APPLES-TO-APPLES CHO NAM)
# ============================================================
library(margins)

# ------------------------------------------------------------
# 3.1. Chạy mô hình Probit cho NAM với cấu trúc y hệt NỮ
# ------------------------------------------------------------
probit_male <- glm(
  LFP ~ Potential_Experience + Experience_Squared + Highest_Qualification + 
    Urban + Marital_Status + as.factor(TINH) + Has_Available_GP, 
  family = binomial(link = "probit"), 
  data = df_male,
  na.action = na.exclude
)

cat("\n=== KẾT QUẢ PROBIT MÔ HÌNH NAM ===\n")
# Kiểm tra nhanh p-value của hệ số thô
summary(probit_male)$coefficients[c("Has_Available_GP"), ]

# ------------------------------------------------------------
# 3.2. Tính AME (Average Marginal Effects) cho 2 biến IV
# ------------------------------------------------------------
cat("\n=== MARGINAL EFFECTS (AME) CỦA IV CHO NAM ===\n")
# Lưu ý: Hàm margins chạy trên mẫu lớn + Fixed Effects có thể mất 1-2 phút
ame_male <- margins(probit_male, variables = c("Has_Available_GP"))
summary(ame_male)
