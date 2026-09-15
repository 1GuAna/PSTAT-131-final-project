# ==============================================================================
# 06_fidelity.R
# PSTAT 131/231 Final Project — 保真度分析（扩展章节 §6）
#
# 在样本量匹配（各 20,000 行）的条件下，比较原始公式数据与 CTGAN 合成数据：
#   (1) 条件关系  —— 同一 recipe 下的 OLS 系数
#   (2) 可预测性  —— 相同交叉验证方案下的 RMSE 与 R²
#   (3) 变量排序  —— 系数绝对值的秩相关
#
# 本脚本不参与模型选择，也不使用任何测试集。 ▸ 不影响 Rubric #14
#
# 输入：results/syn20.rds, results/orig.rds
# 输出：results/fidelity.rds
# ==============================================================================

library(tidyverse)
library(tidymodels)
tidymodels_prefer()

set.seed(131)
syn20 <- readRDS("results/syn20.rds")
orig  <- readRDS("results/orig.rds")

rec_of <- function(d) recipe(exam_score ~ ., data = d) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors())

# ------------------------------------------------------------------------------
# 1. 条件关系：OLS 系数对比
#    ⚠️ 归一化后的系数不可直接与未归一化的相比，两边用同一 recipe 即可对齐。
# ------------------------------------------------------------------------------
coefs_of <- function(d, label) {
  workflow() %>%
    add_model(linear_reg() %>% set_engine("lm")) %>%
    add_recipe(rec_of(d)) %>%
    fit(d) %>%
    extract_fit_parsnip() %>% tidy() %>%
    transmute(term, !!label := estimate)
}

coef_cmp <- full_join(coefs_of(orig,  "original"),
                      coefs_of(syn20, "synthetic"), by = "term") %>%
  filter(term != "(Intercept)") %>%
  mutate(diff  = synthetic - original,
         ratio = synthetic / original) %>%
  arrange(desc(abs(original)))

cat("=== Conditional relationships: OLS coefficients ===\n")
print(as.data.frame(coef_cmp %>% mutate(across(where(is.numeric), ~ round(.x, 4)))))
cat("\n")

# 只看有实质效应的项（|系数| > 1），考察衰减是否方向一致
strong <- coef_cmp %>% filter(abs(original) > 1)
cat(sprintf("Terms with |coefficient| > 1 : %d\n", nrow(strong)))
cat(sprintf("Shrunk toward zero           : %d of %d\n",
            sum(abs(strong$synthetic) < abs(strong$original)), nrow(strong)))
cat(sprintf("Mean ratio (synthetic/original): %.4f\n", mean(strong$ratio)))
cat(sprintf("Mean attenuation             : %.2f%%\n\n",
            100 * (1 - mean(strong$ratio))))

p_coef <- strong %>%
  pivot_longer(c(original, synthetic), names_to = "dataset", values_to = "estimate") %>%
  mutate(term = fct_reorder(term, abs(estimate))) %>%
  ggplot(aes(estimate, term, fill = dataset)) +
  geom_col(position = position_dodge(width = .7), width = .65) +
  geom_vline(xintercept = 0, linewidth = .3) +
  labs(title = "OLS coefficients before and after CTGAN synthesis",
       subtitle = "Terms with an absolute coefficient above 1, matched sample size",
       x = "Coefficient (standardised predictors)", y = NULL, fill = NULL) +
  theme_minimal(base_size = 10) + theme(legend.position = "bottom")

dir.create("figures", showWarnings = FALSE)
print(p_coef); ggsave("figures/6-2_coefficient_comparison.png", p_coef,
                      width = 7.5, height = 5.5, dpi = 150)

# ------------------------------------------------------------------------------
# 2. 可预测性：相同方案下的交叉验证表现
# ------------------------------------------------------------------------------
cv_of <- function(d, label) {
  set.seed(131)
  f <- vfold_cv(d, v = 5, strata = exam_score)
  workflow() %>%
    add_model(linear_reg() %>% set_engine("lm")) %>%
    add_recipe(rec_of(d)) %>%
    fit_resamples(resamples = f, metrics = metric_set(rmse, rsq)) %>%
    collect_metrics() %>%
    transmute(dataset = label, .metric, mean = round(mean, 4),
              std_err = round(std_err, 4))
}

pred_cmp <- bind_rows(cv_of(orig, "Original (formula)"),
                      cv_of(syn20, "Synthetic (CTGAN)")) %>%
  arrange(.metric, dataset)

cat("=== Predictability: 5-fold cross-validated linear model ===\n")
print(as.data.frame(pred_cmp)); cat("\n")

sd_cmp <- tibble(
  dataset = c("Original (formula)", "Synthetic (CTGAN)"),
  sd_outcome = c(sd(orig$exam_score), sd(syn20$exam_score))
) %>% mutate(sd_outcome = round(sd_outcome, 4))
cat("=== Outcome standard deviation (for context) ===\n")
print(as.data.frame(sd_cmp)); cat("\n")

# ------------------------------------------------------------------------------
# 3. 变量排序是否保持
# ------------------------------------------------------------------------------
rank_cor <- cor(abs(coef_cmp$original), abs(coef_cmp$synthetic), method = "spearman")
cat(sprintf("Spearman rank correlation of |coefficients|: %.4f\n\n", rank_cor))

saveRDS(list(coef_cmp = coef_cmp, strong = strong, p_coef = p_coef,
             pred_cmp = pred_cmp, sd_cmp = sd_cmp, rank_cor = rank_cor),
        "results/fidelity.rds")

cat("Saved: results/fidelity.rds\n")
