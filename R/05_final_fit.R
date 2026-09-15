# ==============================================================================
# 05_final_fit.R
# PSTAT 131/231 Final Project — 最优模型的最终拟合与测试集评估
#
# ▸ Rubric #14 (5 分)
#   ⚠️ 零分条件："All models were used to predict the testing set"。
#      本脚本只把跨折 RMSE 最低的那 1 个模型用于测试集。
#      变量重要性在训练集上计算，不涉及测试集，不违反该条。
#
# 输入：results/split.rds, results/tuning.rds
# 输出：results/final_fit.rds
# ==============================================================================

library(tidyverse)
library(tidymodels)
library(vip)   # 注意：vi() 与 utils::vi 撞名，下方统一写作 vip::vi()
tidymodels_prefer()

set.seed(131)
S <- readRDS("results/split.rds")
T <- readRDS("results/tuning.rds")

train <- S$train
test  <- S$test

# ------------------------------------------------------------------------------
# 1. 选出跨折 RMSE 最低的模型
# ------------------------------------------------------------------------------
cat("=== Cross-validated RMSE by model ===\n")
print(as.data.frame(T$summary_tbl)); cat("\n")

best_name <- T$summary_tbl$model[1]
cat("Best model by cross-validated RMSE:", best_name, "\n\n")

key <- c("Linear regression" = "lm", "Elastic net" = "en",
         "K-nearest neighbors" = "knn", "Random forest" = "rf",
         "Boosted tree" = "bt")[[best_name]]

best_wf <- T$wf[[key]]

if (key == "lm") {
  final_wf <- best_wf                       # 无超参数，无需 finalize
  best_par <- NULL
} else {
  best_par <- select_best(T[[key]], metric = "rmse")
  final_wf <- finalize_workflow(best_wf, best_par)
  cat("Selected hyperparameters:\n"); print(as.data.frame(best_par)); cat("\n")
}

# ------------------------------------------------------------------------------
# 2. 在完整训练集上拟合，并在测试集上评估（测试集只用这一次）
# ------------------------------------------------------------------------------
final_fit <- fit(final_wf, data = train)
preds     <- augment(final_fit, new_data = test)

test_metrics <- bind_rows(
  rmse(preds, truth = exam_score, estimate = .pred),
  rsq(preds,  truth = exam_score, estimate = .pred),
  mae(preds,  truth = exam_score, estimate = .pred)
) %>% mutate(.estimate = round(.estimate, 4))

cat("=== Test-set performance:", best_name, "===\n")
print(as.data.frame(test_metrics)); cat("\n")

cv_rmse <- T$summary_tbl$mean[1]
cat(sprintf("Cross-validated RMSE : %.4f\n", cv_rmse))
cat(sprintf("Testing RMSE         : %.4f\n",
            test_metrics$.estimate[test_metrics$.metric == "rmse"]))
cat(sprintf("Difference           : %+.4f\n\n",
            test_metrics$.estimate[test_metrics$.metric == "rmse"] - cv_rmse))

# ------------------------------------------------------------------------------
# 3. 残差诊断 —— 截断边界应在此清晰可见
# ------------------------------------------------------------------------------
p_resid <- preds %>%
  mutate(.resid = exam_score - .pred) %>%
  ggplot(aes(.pred, .resid)) +
  geom_point(alpha = .15, size = .6) +
  geom_hline(yintercept = 0, colour = "firebrick", linewidth = .6) +
  labs(title = "Residuals against fitted values",
       subtitle = "Diagonal edges reflect censoring of the outcome at 19.6 and 100",
       x = "Predicted exam score", y = "Residual") +
  theme_minimal(base_size = 11)

p_obs_pred <- preds %>%
  ggplot(aes(.pred, exam_score)) +
  geom_point(alpha = .15, size = .6) +
  geom_abline(slope = 1, intercept = 0, colour = "firebrick", linewidth = .6) +
  coord_equal() +
  labs(title = "Observed against predicted exam score",
       subtitle = "Red line: perfect prediction",
       x = "Predicted", y = "Observed") +
  theme_minimal(base_size = 11)

dir.create("figures", showWarnings = FALSE)
print(p_resid);    ggsave("figures/5-3_residuals.png",    p_resid,    width = 7, height = 4.5, dpi = 150)
print(p_obs_pred); ggsave("figures/5-3_obs_vs_pred.png",  p_obs_pred, width = 5.5, height = 5.5, dpi = 150)

# 截断区域的残差结构：预测值接近上界时残差被迫为负
cat("=== Residual summary by region of the outcome ===\n")
print(preds %>%
        mutate(.resid = exam_score - .pred,
               region = case_when(exam_score == 100 ~ "At upper bound (100)",
                                  exam_score <= 20  ~ "At lower bound",
                                  TRUE              ~ "Interior")) %>%
        group_by(region) %>%
        summarise(n = n(), mean_resid = round(mean(.resid), 3),
                  sd_resid = round(sd(.resid), 3)) %>% as.data.frame())
cat("\n")

# ------------------------------------------------------------------------------
# 4. 变量重要性
#    用随机森林的最优设定在**训练集**上拟合一次，仅用于重要性度量。
#    这不涉及测试集，因此不违反 Rubric #14。
# ------------------------------------------------------------------------------
rf_fit <- finalize_workflow(T$wf$rf, select_best(T$rf, metric = "rmse")) %>%
  fit(data = train)

p_vip <- rf_fit %>% extract_fit_parsnip() %>%
  vip(num_features = S$n_pred, geom = "col") +
  labs(title = "Variable importance (random forest, impurity)",
       subtitle = "Fitted on the training set only") +
  theme_minimal(base_size = 10)

print(p_vip); ggsave("figures/5-4_vip.png", p_vip, width = 7, height = 6, dpi = 150)

vip_tbl <- rf_fit %>% extract_fit_parsnip() %>%
  vip::vi() %>% mutate(Importance = round(Importance, 1))
cat("=== Variable importance (top 12) ===\n")
print(as.data.frame(head(vip_tbl, 12))); cat("\n")

# ------------------------------------------------------------------------------
# 5. 存盘
# ------------------------------------------------------------------------------
saveRDS(list(best_name = best_name, best_par = best_par,
             final_fit = final_fit, preds = preds,
             test_metrics = test_metrics, cv_rmse = cv_rmse,
             p_resid = p_resid, p_obs_pred = p_obs_pred,
             p_vip = p_vip, vip_tbl = vip_tbl),
        "results/final_fit.rds")

cat("Saved: results/final_fit.rds\n")
