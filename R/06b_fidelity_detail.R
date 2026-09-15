# ==============================================================================
# 06b_fidelity_detail.R
# §6 的三项补充计算。依赖 06_fidelity.R 已生成的 results/fidelity.rds。
#
#   (1) 排序保持度应分开算 —— 11 个实质项 vs 12 个噪声项
#   (2) 噪声项的系数是否被放大，以及是否达到统计显著
#   (3) 方差分解 —— R² 上升到底来自信号增强还是噪声减少
# ==============================================================================

library(tidyverse)
library(tidymodels)
tidymodels_prefer()

syn20 <- readRDS("results/syn20.rds")
orig  <- readRDS("results/orig.rds")
F     <- readRDS("results/fidelity.rds")

rec_of <- function(d) recipe(exam_score ~ ., data = d) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors())

# ------------------------------------------------------------------------------
# 1. 排序保持度：实质项与噪声项分开
#    对系数本就为零的项计算排序相关没有意义 —— 那是在给随机噪声排名。
# ------------------------------------------------------------------------------
strong <- F$coef_cmp %>% filter(abs(original) > 1)
weak   <- F$coef_cmp %>% filter(abs(original) <= 1)

rc <- function(d) cor(abs(d$original), abs(d$synthetic), method = "spearman")

cat("=== Rank preservation of |coefficients| ===\n")
cat(sprintf("All %2d terms          : %.4f\n", nrow(F$coef_cmp), F$rank_cor))
cat(sprintf("Substantive (%2d terms): %.4f\n", nrow(strong), rc(strong)))
cat(sprintf("Null        (%2d terms): %.4f\n\n", nrow(weak), rc(weak)))

# ------------------------------------------------------------------------------
# 2. 噪声项：系数被放大了吗？是否达到显著？
# ------------------------------------------------------------------------------
cat("=== Null terms: magnitude ===\n")
cat(sprintf("Mean |coefficient|, original : %.4f\n", mean(abs(weak$original))))
cat(sprintf("Mean |coefficient|, synthetic: %.4f\n", mean(abs(weak$synthetic))))
cat(sprintf("Inflation factor             : %.2fx\n\n",
            mean(abs(weak$synthetic)) / mean(abs(weak$original))))

tidy_of <- function(d, label) {
  workflow() %>%
    add_model(linear_reg() %>% set_engine("lm")) %>%
    add_recipe(rec_of(d)) %>%
    fit(d) %>% extract_fit_parsnip() %>% tidy() %>%
    filter(term != "(Intercept)") %>%
    transmute(term, estimate, std.error, statistic, p.value, dataset = label)
}

signif_tbl <- bind_rows(tidy_of(orig, "Original"), tidy_of(syn20, "Synthetic")) %>%
  filter(term %in% weak$term) %>%
  mutate(significant = p.value < 0.05,
         across(c(estimate, std.error, statistic), ~ round(.x, 4)),
         p.value = round(p.value, 4)) %>%
  arrange(term, dataset)

cat("=== Null terms: statistical significance at n = 20,000 ===\n")
print(as.data.frame(signif_tbl)); cat("\n")

cat("Null terms significant at the 5% level:\n")
print(signif_tbl %>% count(dataset, significant) %>% as.data.frame()); cat("\n")

# ------------------------------------------------------------------------------
# 3. 方差分解：R² 上升来自信号增强还是噪声减少？
# ------------------------------------------------------------------------------
decomp <- function(d, label) {
  f   <- workflow() %>% add_model(linear_reg() %>% set_engine("lm")) %>%
    add_recipe(rec_of(d)) %>% fit(d)
  p   <- predict(f, d)$.pred
  res <- d$exam_score - p
  tibble(dataset = label,
         var_total = var(d$exam_score), var_fitted = var(p), var_resid = var(res),
         r_squared = 1 - var(res) / var(d$exam_score))
}

var_tbl <- bind_rows(decomp(orig, "Original (formula)"),
                     decomp(syn20, "Synthetic (CTGAN)")) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))

cat("=== Variance decomposition ===\n")
print(as.data.frame(var_tbl)); cat("\n")
cat(sprintf("Change in fitted variance  : %+.2f%%\n",
            100 * (var_tbl$var_fitted[2] / var_tbl$var_fitted[1] - 1)))
cat(sprintf("Change in residual variance: %+.2f%%\n\n",
            100 * (var_tbl$var_resid[2] / var_tbl$var_resid[1] - 1)))

F$rank_strong <- rc(strong); F$rank_weak <- rc(weak)
F$weak <- weak; F$signif_tbl <- signif_tbl; F$var_tbl <- var_tbl
saveRDS(F, "results/fidelity.rds")
cat("Updated: results/fidelity.rds\n")
