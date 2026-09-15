# ==============================================================================
# 02_eda.R
# PSTAT 131/231 Final Project — Exploratory Data Analysis
#
# Consolidated version of the original 02_eda.R plus the corrections previously
# applied in 02b_fix_and_render.R and 02c_fix_histogram.R.
#
# Inputs:
#   results/syn20.rds
#   results/orig.rds
#
# Outputs:
#   results/eda.rds
#   figures/2-1_outcome_histogram.png
#   figures/2-3_correlation_matrix.png
#   figures/2-4_study_hours_shape.png
#   figures/2-5_categorical_boxplots.png
#   figures/2-7_marginal_distributions.png
# ==============================================================================

library(tidyverse)
library(corrplot)

set.seed(131)

syn20 <- readRDS("results/syn20.rds")
orig  <- readRDS("results/orig.rds")

dir.create("figures", showWarnings = FALSE)
theme_set(theme_minimal(base_size = 11))

# ==============================================================================
# 2.1 Distribution of the outcome
# Use a 2-point bin width aligned to 20. Because exam_score is recorded on a
# 0.1-point grid, this avoids the alternating-bin artifact produced by bins = 60.
# ==============================================================================
p_hist <- bind_rows(
  orig  %>% transmute(exam_score, source = "Original (formula, n = 20,000)"),
  syn20 %>% transmute(exam_score, source = "Synthetic (CTGAN, n = 20,000)")
) %>%
  ggplot(aes(exam_score)) +
  geom_histogram(
    binwidth = 2, boundary = 20,
    fill = "steelblue", colour = "white", linewidth = .2
  ) +
  facet_wrap(~ source) +
  labs(
    title = "Distribution of exam_score",
    subtitle = "Bin width 2 points; note the pile-ups at both bounds",
    x = "Exam score", y = "Count"
  )

cat("=== 2.1 Censoring counts ===\n")
censor_tbl <- bind_rows(
  syn20 %>% summarise(
    source = "Synthetic", n = n(),
    at_max = sum(exam_score == 100),
    at_min = sum(exam_score == min(exam_score)),
    min = min(exam_score), max = max(exam_score)
  ),
  orig %>% summarise(
    source = "Original", n = n(),
    at_max = sum(exam_score == 100),
    at_min = sum(exam_score == min(exam_score)),
    min = min(exam_score), max = max(exam_score)
  )
) %>%
  mutate(pct_at_max = round(100 * at_max / n, 2))
print(censor_tbl)
cat("\n")

# ==============================================================================
# 2.2 Missing data
# ==============================================================================
cat("=== 2.2 Missing data ===\n")
missing_tbl <- bind_rows(
  syn20 %>%
    summarise(across(everything(), ~ sum(is.na(.x)))) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "n_missing") %>%
    mutate(dataset = "Synthetic"),
  orig %>%
    summarise(across(everything(), ~ sum(is.na(.x)))) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "n_missing") %>%
    mutate(dataset = "Original")
) %>%
  pivot_wider(names_from = dataset, values_from = n_missing)

print(missing_tbl)
cat(
  "\nTotal missing values across both datasets:",
  sum(is.na(syn20)) + sum(is.na(orig)), "\n\n"
)

# ==============================================================================
# 2.3 Correlation structure among numeric variables
# ==============================================================================
num_mat <- syn20 %>%
  select(where(is.numeric)) %>%
  cor()

cat("=== 2.3 Correlations with exam_score ===\n")
print(round(num_mat[, "exam_score"], 4))
cat("\n")

# ==============================================================================
# 2.4 Shape of the strongest relationship
# ==============================================================================
bin_means <- syn20 %>%
  mutate(bin = cut(study_hours, breaks = 16)) %>%
  group_by(bin) %>%
  summarise(
    mid = mean(study_hours),
    mean_score = mean(exam_score),
    n = n(),
    .groups = "drop"
  )

p_shape <- ggplot(bin_means, aes(mid, mean_score)) +
  geom_line(colour = "grey40") +
  geom_point(size = 2, colour = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, linetype = 2, colour = "firebrick") +
  labs(
    title = "Binned mean exam_score against study_hours",
    subtitle = "Dashed line: ordinary least-squares fit",
    x = "study_hours (bin midpoint)", y = "Mean exam score"
  )

cat("=== 2.4 Binned means of study_hours ===\n")
print(
  as.data.frame(
    bin_means %>% mutate(across(where(is.numeric), ~ round(.x, 2)))
  )
)
cat("\n")

# ==============================================================================
# 2.5 Categorical predictors
# ==============================================================================
cat_vars <- syn20 %>%
  select(where(is.factor)) %>%
  names()

p_box <- syn20 %>%
  select(exam_score, all_of(cat_vars)) %>%
  pivot_longer(-exam_score, names_to = "variable", values_to = "level") %>%
  ggplot(aes(level, exam_score)) +
  geom_boxplot(fill = "steelblue", alpha = .5, outlier.size = .3) +
  facet_wrap(~ variable, scales = "free_x", ncol = 3) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1, size = 7)) +
  labs(
    title = "Exam score by categorical predictor",
    x = NULL, y = "Exam score"
  )

cat("=== 2.5 Group means by categorical predictor ===\n")
for (v in cat_vars) {
  cat("--", v, "--\n")
  print(
    syn20 %>%
      group_by(.data[[v]]) %>%
      summarise(
        n = n(),
        mean = round(mean(exam_score), 2),
        sd = round(sd(exam_score), 2),
        .groups = "drop"
      ) %>%
      as.data.frame()
  )
}
cat("\n")

# ==============================================================================
# 2.6 Signal strength across all predictors
# Numeric variables use Pearson correlation. Categorical variables use the range
# of group means in exam-score points.
#
# This implements the corrected calculation directly, so no separate 02b patch
# is needed.
# ==============================================================================
num_vars <- setdiff(
  names(select(syn20, where(is.numeric))),
  "exam_score"
)

num_stat <- map_dbl(
  num_vars,
  ~ cor(syn20[[.x]], syn20$exam_score)
)

cat_stat <- map_dbl(cat_vars, function(v) {
  m <- tapply(syn20$exam_score, syn20[[v]], mean)
  max(m) - min(m)
})

signal_tbl <- bind_rows(
  tibble(
    variable = num_vars,
    statistic = num_stat,
    measure = "Pearson r",
    strength = abs(num_stat)
  ),
  tibble(
    variable = cat_vars,
    statistic = cat_stat,
    measure = "Group mean range (points)",
    strength = cat_stat
  )
) %>%
  arrange(desc(strength)) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))

cat("=== 2.6 Signal strength summary ===\n")
print(as.data.frame(signal_tbl))
cat("\n")

# ==============================================================================
# 2.7 Comparing the two datasets
# ==============================================================================
both <- bind_rows(
  syn20 %>% mutate(source = "Synthetic (CTGAN)"),
  orig  %>% mutate(source = "Original (formula)")
)

p_kde <- both %>%
  select(source, where(is.numeric)) %>%
  pivot_longer(-source, names_to = "variable", values_to = "value") %>%
  ggplot(aes(value, colour = source)) +
  geom_density(linewidth = .7) +
  facet_wrap(~ variable, scales = "free", ncol = 3) +
  labs(
    title = "Marginal distributions: original formula data vs CTGAN synthesis",
    x = NULL, y = "Density", colour = NULL
  ) +
  theme(legend.position = "bottom")

cat("=== 2.7 Quantile comparison ===\n")
quant_tbl <- both %>%
  select(source, where(is.numeric)) %>%
  pivot_longer(-source, names_to = "variable", values_to = "value") %>%
  group_by(variable, source) %>%
  summarise(
    min = min(value),
    q25 = quantile(value, .25),
    median = median(value),
    q75 = quantile(value, .75),
    max = max(value),
    mean = mean(value),
    sd = sd(value),
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

print(as.data.frame(quant_tbl))
cat("\n")

# ==============================================================================
# Export figures
# ==============================================================================
print(p_hist)
ggsave(
  "figures/2-1_outcome_histogram.png", p_hist,
  width = 8, height = 4, dpi = 150
)

# corrplot uses base graphics, so save it through a PNG graphics device.
png(
  "figures/2-3_correlation_matrix.png",
  width = 1200, height = 1200, res = 180
)
corrplot(
  num_mat, method = "circle", type = "lower",
  addCoef.col = "black", tl.col = "black",
  diag = FALSE, number.cex = .8
)
dev.off()

# Also draw it in the active graphics device when the script is run interactively.
corrplot(
  num_mat, method = "circle", type = "lower",
  addCoef.col = "black", tl.col = "black",
  diag = FALSE, number.cex = .8
)

print(p_shape)
ggsave(
  "figures/2-4_study_hours_shape.png", p_shape,
  width = 7, height = 4.5, dpi = 150
)

print(p_box)
ggsave(
  "figures/2-5_categorical_boxplots.png", p_box,
  width = 9, height = 7, dpi = 150
)

print(p_kde)
ggsave(
  "figures/2-7_marginal_distributions.png", p_kde,
  width = 9, height = 5, dpi = 150
)

# ==============================================================================
# Save every object used by final_project.Rmd in one RDS file.
# ==============================================================================
saveRDS(
  list(
    p_hist = p_hist,
    p_shape = p_shape,
    p_box = p_box,
    p_kde = p_kde,
    num_mat = num_mat,
    censor_tbl = censor_tbl,
    missing_tbl = missing_tbl,
    bin_means = bin_means,
    signal_tbl = signal_tbl,
    quant_tbl = quant_tbl
  ),
  "results/eda.rds"
)

cat("Saved: results/eda.rds\n")
cat("Saved five EDA figures to figures/\n")
