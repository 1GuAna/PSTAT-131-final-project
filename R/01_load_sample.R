# ==============================================================================
# 01_load_sample.R
# PSTAT 131/231 Final Project
#
# 目的：读入两份数据集，统一结构，从竞赛合成数据中分层抽取 20,000 行，
#      使其与对照组（原始公式数据）的样本量匹配，并存盘供后续脚本使用。
#
# 输入：data/train.csv                    (Kaggle PS S6E1, 630,000 行)
#      data/Exam_Score_Prediction.csv    (原始公式数据, 20,000 行)
# 输出：results/syn20.rds  — primary modeling dataset (20,000 行)
#      results/orig.rds   — comparison arm          (20,000 行)
# ==============================================================================

library(tidyverse)

set.seed(131)
dir.create("results", showWarnings = FALSE)

# ------------------------------------------------------------------------------
# 1. 读入
# ------------------------------------------------------------------------------
syn_raw  <- read_csv("data/train.csv",                 show_col_types = FALSE)
orig_raw <- read_csv("data/Exam_Score_Prediction.csv", show_col_types = FALSE)

cat("Synthetic (competition) :", nrow(syn_raw),  "x", ncol(syn_raw),  "\n")
cat("Original  (formula)     :", nrow(orig_raw), "x", ncol(orig_raw), "\n\n")

# ------------------------------------------------------------------------------
# 2. 统一结构
#    两份数据的 ID 列名不同：竞赛数据是 id，原始数据是 student_id。两者都丢弃。
# ------------------------------------------------------------------------------

# 有序变量的水平顺序（仅为作图排序，**不设为 ordered factor**）
#
# ⚠️ 重要：不要用 factor(..., ordered = TRUE)。
#    recipes::step_dummy() 对 ordered factor 默认使用多项式对比（.L/.Q/.C），
#    会生成难以解释的线性/二次项，而不是我们想要的指示变量。
#    用 levels = 指定顺序即可获得作图所需的排序，同时保持普通 factor。
lv_sleep  <- c("poor", "average", "good")
lv_facil  <- c("low", "medium", "high")
lv_diff   <- c("easy", "moderate", "hard")

harmonise <- function(df) {
  df %>%
    select(-any_of(c("id", "student_id"))) %>%
    mutate(
      across(where(is.character), factor),
      sleep_quality   = factor(sleep_quality,   levels = lv_sleep),
      facility_rating = factor(facility_rating, levels = lv_facil),
      exam_difficulty = factor(exam_difficulty, levels = lv_diff)
    )
}

syn  <- harmonise(syn_raw)
orig <- harmonise(orig_raw)

# 确认两份数据同构 —— 若此处报错，说明列名或变量类型不一致，需先处理
stopifnot(setequal(names(syn), names(orig)))
orig <- orig %>% select(all_of(names(syn)))   # 统一列顺序

cat("Columns harmonised:\n"); print(names(syn)); cat("\n")

# ------------------------------------------------------------------------------
# 3. 分层抽样：从 630,000 行中按 exam_score 的十分位抽取 20,000 行
#    理由：(a) 与对照组 n 匹配，消除样本量对模型比较的混淆；
#          (b) 使 5 个模型的交叉验证调参在单机上可行。
# ------------------------------------------------------------------------------
syn20 <- syn %>%
  mutate(.bin = ntile(exam_score, 10)) %>%
  group_by(.bin) %>%
  slice_sample(n = 2000) %>%
  ungroup() %>%
  select(-.bin)

cat("Stratified sample drawn:", nrow(syn20), "rows\n\n")

# 抽样保真度检查：子样本与总体的 outcome 分位数应几乎一致
cat("--- exam_score quantiles: full synthetic vs 20k sample vs original ---\n")
qs <- c(0, .25, .5, .75, 1)
print(bind_rows(
  tibble(source = "Synthetic (full 630k)", t(quantile(syn$exam_score,   qs))),
  tibble(source = "Synthetic (20k sample)", t(quantile(syn20$exam_score, qs))),
  tibble(source = "Original (20k)",         t(quantile(orig$exam_score,  qs)))
))
cat("\n")

# ------------------------------------------------------------------------------
# 4. 存盘
# ------------------------------------------------------------------------------
saveRDS(syn20, "results/syn20.rds")
saveRDS(orig,  "results/orig.rds")

cat("Saved: results/syn20.rds, results/orig.rds\n")
