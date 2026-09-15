# ==============================================================================
# 05c_importance_vs_signal.R
# §5.4 补充：把不纯度重要性排名与"单变量解释力"放在同一把尺子上比较。
#
# §2.6 对数值变量用 Pearson r、对分类变量用组均值极差，两者不可直接比较。
# 此处改用单变量 R²（数值变量为 r²，分类变量为 eta²），单位统一为"解释的方差比例"，
# 从而可以检验：重要性排名究竟在追踪真实解释力，还是在追踪变量的取值个数？
# ==============================================================================

library(tidyverse)

syn20 <- readRDS("results/syn20.rds")
FIT   <- readRDS("results/final_fit.rds")

y <- syn20$exam_score; tot <- var(y)

single_r2 <- map_dfr(setdiff(names(syn20), "exam_score"), function(v) {
  x <- syn20[[v]]
  r2 <- if (is.numeric(x)) {
    cor(x, y)^2
  } else {
    g <- tibble(x, y) %>% group_by(x) %>%
      summarise(n = n(), v = var(y), .groups = "drop")
    within <- sum(g$v * (g$n - 1)) / (nrow(syn20) - nrow(g))
    1 - within / tot
  }
  tibble(source_var = v, n_distinct = n_distinct(x), single_r2 = r2)
})

# 每个原始变量在重要性排名中的最佳名次
best_rank <- FIT$vip_diag %>%
  group_by(source_var) %>%
  summarise(importance_rank = min(rank), .groups = "drop")

cmp <- left_join(single_r2, best_rank, by = "source_var") %>%
  arrange(importance_rank) %>%
  mutate(single_r2 = round(single_r2, 4))

cat("=== Importance rank against explanatory power and cardinality ===\n")
print(as.data.frame(cmp)); cat("\n")

sp_r2   <- cor(cmp$importance_rank, -cmp$single_r2,  method = "spearman")
sp_card <- cor(cmp$importance_rank, -cmp$n_distinct, method = "spearman")

cat(sprintf("Spearman(importance rank, explanatory power): %.3f\n", sp_r2))
cat(sprintf("Spearman(importance rank, cardinality)     : %.3f\n\n", sp_card))

p_imp_signal <- cmp %>%
  ggplot(aes(single_r2, importance_rank, size = n_distinct)) +
  geom_point(alpha = .75, colour = "steelblue") +
  ggrepel::geom_text_repel(aes(label = source_var), size = 3, seg.color = "grey60") +
  scale_y_reverse(breaks = 1:11) +
  scale_size_continuous(range = c(2, 9), name = "Distinct values") +
  labs(title = "Importance rank against single-predictor explanatory power",
       subtitle = "Point size shows the number of distinct values a predictor takes",
       x = expression("Variance of the outcome explained alone (" * R^2 * ")"),
       y = "Rank in the importance table") +
  theme_minimal(base_size = 10)

print(p_imp_signal)
ggsave("figures/5-4_importance_vs_signal.png", p_imp_signal,
       width = 7, height = 5, dpi = 150)

FIT$imp_cmp <- cmp; FIT$sp_r2 <- sp_r2; FIT$sp_card <- sp_card
FIT$p_imp_signal <- p_imp_signal
saveRDS(FIT, "results/final_fit.rds")
cat("Updated: results/final_fit.rds\n")
