# ============================================================
# BƯỚC 2: SELECTION EQUATION (PROBIT) & INVERSE MILLS RATIO
# ============================================================
library(car)

# ------------------------------------------------------------
# 1. Chạy mô hình Probit cho NỮ 
# (Thêm na.action = na.exclude để chống lệch dòng)
# ------------------------------------------------------------
probit_female <- glm(
  LFP ~ Potential_Experience + Experience_Squared + Highest_Qualification + 
    Urban + Marital_Status + 
    HH_Dependency_Ratio + effective_gp_support, 
  family = binomial(link = "probit"), 
  data = df_female,
  na.action = na.exclude # << BẢN VÁ LỖI Ở ĐÂY
)

cat("=== KẾT QUẢ PROBIT MÔ HÌNH NỮ ===\n")
summary(probit_female)

# ------------------------------------------------------------
# 2. Kiểm định sức mạnh biến công cụ (NỮ)
# ------------------------------------------------------------
cat("\n=== WALD TEST: KIỂM ĐỊNH SỨC MẠNH BIẾN CÔNG CỤ (NỮ) ===\n")
iv_test_female <- linearHypothesis(
  probit_female, 
  c("HH_Dependency_Ratio = 0", "effective_gp_support = 0")
)
print(iv_test_female)

# ------------------------------------------------------------
# 3. Tính Inverse Mills Ratio ($\lambda$) cho Nữ
# ------------------------------------------------------------
Z_gamma_female <- predict(probit_female, type = "link")

# Bây giờ Z_gamma_female đã có chuẩn xác 188.602 dòng (có chứa NA)
# Việc gán sẽ diễn ra trơn tru tuyệt đối
df_female$IMR <- dnorm(Z_gamma_female) / pnorm(Z_gamma_female)

# ------------------------------------------------------------
# 4. Lặp lại quy trình y hệt cho NAM
# ------------------------------------------------------------
probit_male <- glm(
  LFP ~ Potential_Experience + Experience_Squared + Highest_Qualification + 
    Urban + Marital_Status + 
    HH_Dependency_Ratio + effective_gp_support,
  family = binomial(link = "probit"), 
  data = df_male,
  na.action = na.exclude # << BẢN VÁ LỖI CHO NAM
)

cat("\n=== KẾT QUẢ PROBIT MÔ HÌNH NAM ===\n")
summary(probit_male)

cat("\n=== WALD TEST: KIỂM ĐỊNH SỨC MẠNH BIẾN CÔNG CỤ (NAM) ===\n")
iv_test_male <- linearHypothesis(
  probit_male, 
  c("HH_Dependency_Ratio = 0", "effective_gp_support = 0")
)
print(iv_test_male)

# Tính \lambda cho Nam
Z_gamma_male <- predict(probit_male, type = "link")
df_male$IMR <- dnorm(Z_gamma_male) / pnorm(Z_gamma_male)

# ------------------------------------------------------------
# 5. Cập nhật lại \lambda vào bộ dữ liệu người đi làm (df_workers)
# ------------------------------------------------------------
df_with_imr <- bind_rows(df_female, df_male)

df_workers <- df_workers %>%
  select(-any_of(c("IMR", "IMR.x", "IMR.y"))) %>%
  left_join(
    df_with_imr %>% select(ID_CA_NHAN, IMR), 
    by = "ID_CA_NHAN"
  )

cat("\n=== KIỂM TRA IMR TRONG DỮ LIỆU ĐI LÀM ===\n")
summary(df_workers$IMR)

