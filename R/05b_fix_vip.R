# ==============================================================================
# 05b_fix_vip.R
# 重建变量重要性图，并加上不纯度重要性偏差的诊断。
#
# 问题：vip() 在此环境下返回的不是 ggplot 对象（print 输出 NULL），
#       渲染成 base graphics，标签重叠不可用。
# 解法：直接用已保存的 vip_tbl 手工绘制 ggplot。
# ==============================================================================

library(tidyverse)

FIT   <- readRDS("results/final_fit.rds")
syn20 <- readRDS("results/syn20.rds")

vip_tbl <- FIT$vip_tbl

# ------------------------------------------------------------------------------
# 1. 重建重要性图
# ------------------------------------------------------------------------------
p_vip <- vip_tbl %>%
  mutate(Variable = fct_reorder(Variable, Importance)) %>%
  ggplot(aes(Importance, Variable)) +
  geom_col(fill = "steelblue", width = .7) +
  scale_x_continuous(labels = scales::label_number(scale = 1e-6, suffix = "M")) +
  labs(title = "Variable importance (random forest, impurity)",
       subtitle = "Fitted on the training set only",
       x = "Impurity-based importance", y = NULL) +
  theme_minimal(base_size = 10)

print(p_vip)
ggsave("figures/5-4_vip.png", p_vip, width = 7, height = 6, dpi = 150)

# ------------------------------------------------------------------------------
# 2. 诊断：重要性是否被"取值个数"污染？
#
#    不纯度重要性对可切分点多的变量存在系统性偏好。`age` 在 §2.6 中被判定为
#    纯噪声（r = 0.010），若它在重要性排名中靠前，即为该偏差的直接证据。
# ------------------------------------------------------------------------------
n_levels <- syn20 %>%
  summarise(across(-exam_score, n_distinct)) %>%
  pivot_longer(everything(), names_to = "source_var", values_to = "n_distinct")

signal <- readRDS("results/eda.rds")$signal_tbl %>%
  select(source_var = variable, signal_strength = strength, measure)

# 把指示变量映射回其原始变量
map_back <- function(v) {
  hit <- n_levels$source_var[map_lgl(n_levels$source_var, ~ startsWith(v, .x))]
  if (length(hit) == 0) v else hit[which.max(nchar(hit))]
}

vip_diag <- vip_tbl %>%
  mutate(source_var = map_chr(Variable, map_back),
         rank = row_number()) %>%
  left_join(n_levels, by = "source_var") %>%
  left_join(signal, by = "source_var") %>%
  select(rank, Variable, source_var, n_distinct, Importance,
         signal_strength, measure)

cat("=== Variable importance with number of distinct values ===\n")
print(as.data.frame(head(vip_diag, 14))); cat("\n")

cat("=== The `age` anomaly ===\n")
print(as.data.frame(vip_diag %>% filter(source_var == "age"))); cat("\n")

FIT$p_vip <- p_vip; FIT$vip_diag <- vip_diag
saveRDS(FIT, "results/final_fit.rds")
cat("Updated: results/final_fit.rds\n")
