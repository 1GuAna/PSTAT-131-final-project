# ==============================================================================
# 06c_categorical_marginals.R
# §6.1 补充：分类变量的边际分布是否被保留？
#
# §2.7 只比较了数值变量的边际分布。系数分析中出现的虚假信号，
# 可能与分类变量的边际分布被扭曲有关，此处直接检验。
# ==============================================================================

library(tidyverse)

syn20 <- readRDS("results/syn20.rds")
orig  <- readRDS("results/orig.rds")
F     <- readRDS("results/fidelity.rds")

cat_vars <- syn20 %>% select(where(is.factor)) %>% names()

prop_of <- function(d, label) {
  map_dfr(cat_vars, function(v) {
    d %>% count(.data[[v]], name = "n") %>%
      rename(level = 1) %>%
      mutate(variable = v, pct = 100 * n / sum(n), dataset = label)
  })
}

cat_cmp <- bind_rows(prop_of(orig, "original"), prop_of(syn20, "synthetic")) %>%
  select(variable, level, dataset, pct) %>%
  pivot_wider(names_from = dataset, values_from = pct) %>%
  mutate(diff = synthetic - original,
         across(where(is.numeric), ~ round(.x, 2)))

cat("=== Categorical marginals: percentage by level ===\n")
print(as.data.frame(cat_cmp)); cat("\n")

# 每个变量的总变差距离（TVD）：0 表示完全一致，数值越大扭曲越严重
tvd <- cat_cmp %>%
  group_by(variable) %>%
  summarise(tvd = round(sum(abs(diff)) / 2, 2), .groups = "drop") %>%
  arrange(desc(tvd))

cat("=== Total variation distance by variable (percentage points) ===\n")
print(as.data.frame(tvd)); cat("\n")

p_cat <- cat_cmp %>%
  pivot_longer(c(original, synthetic), names_to = "dataset", values_to = "pct") %>%
  ggplot(aes(level, pct, fill = dataset)) +
  geom_col(position = position_dodge(width = .75), width = .7) +
  facet_wrap(~ variable, scales = "free_x", ncol = 3) +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1, size = 7),
        legend.position = "bottom") +
  labs(title = "Categorical marginals before and after CTGAN synthesis",
       x = NULL, y = "Percentage of observations", fill = NULL)

print(p_cat)
ggsave("figures/6-1_categorical_marginals.png", p_cat,
       width = 9, height = 6.5, dpi = 150)

F$cat_cmp <- cat_cmp; F$tvd <- tvd; F$p_cat <- p_cat
saveRDS(F, "results/fidelity.rds")
cat("Updated: results/fidelity.rds\n")
