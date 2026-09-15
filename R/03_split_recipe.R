# ==============================================================================
# 03_split_recipe.R
# PSTAT 131/231 Final Project — 数据划分、重抽样与预处理
#
# 输入：results/syn20.rds
# 输出：results/split.rds  (split / train / test / folds / recipe)
# ==============================================================================

library(tidyverse)
library(tidymodels)
tidymodels_prefer()

set.seed(131)
syn20 <- readRDS("results/syn20.rds")

# ------------------------------------------------------------------------------
# 1. 训练 / 测试划分 —— 80 / 20，分层于 outcome  ▸ Rubric #8 + #9
# ------------------------------------------------------------------------------
split <- initial_split(syn20, prop = 0.80, strata = exam_score)
train <- training(split)
test  <- testing(split)

cat("Training:", nrow(train), " Testing:", nrow(test), "\n\n")

# 分层是否奏效：两个集合的 outcome 分布应几乎一致
cat("=== Outcome distribution: train vs test ===\n")
print(bind_rows(
  train %>% summarise(set = "train", n = n(), mean = mean(exam_score),
                      sd = sd(exam_score), q25 = quantile(exam_score, .25),
                      median = median(exam_score), q75 = quantile(exam_score, .75)),
  test  %>% summarise(set = "test",  n = n(), mean = mean(exam_score),
                      sd = sd(exam_score), q25 = quantile(exam_score, .25),
                      median = median(exam_score), q75 = quantile(exam_score, .75))
) %>% mutate(across(where(is.numeric), ~ round(.x, 3))) %>% as.data.frame())
cat("\n")

# ------------------------------------------------------------------------------
# 2. k 折交叉验证 —— 折内同样分层  ▸ Rubric #11 + #9
#    ⚠️ 分层必须在划分和重抽样两处都做，只做一处按半分计。
#
#    折数在此处统一定义，04 与报告都从 results/split.rds 读取同一组折，
#    避免"报告里写 10 折、实际跑的是 5 折"这类不一致。
# ------------------------------------------------------------------------------
N_FOLDS <- 5

folds <- vfold_cv(train, v = N_FOLDS, strata = exam_score)
cat("Folds created:", nrow(folds), "\n")
cat("Rows per assessment fold:", round(nrow(train) / N_FOLDS), "\n\n")

# ------------------------------------------------------------------------------
# 3. Recipe  ▸ Rubric #10
#    step_dummy() 使用默认的 k-1 指示变量编码（非 one-hot），
#    避免设计矩阵降秩、X'X 奇异 —— 半分条件明确点名了这一点。
#    step_normalize() 服务 elastic net（惩罚项对尺度敏感）与 KNN（距离度量）。
# ------------------------------------------------------------------------------
rec <- recipe(exam_score ~ ., data = train) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors())

baked <- prep(rec) %>% bake(new_data = NULL)
cat("Predictors after preprocessing:", ncol(baked) - 1, "\n")
cat("Columns:\n"); print(setdiff(names(baked), "exam_score")); cat("\n")

# 检查：归一化后所有数值预测变量的均值应 ≈ 0、标准差 ≈ 1
cat("=== Normalisation check (first 6 predictors) ===\n")
print(baked %>% select(-exam_score) %>%
        summarise(across(everything(), list(mean = mean, sd = sd))) %>%
        pivot_longer(everything(),
                     names_to = c("variable", ".value"),
                     names_pattern = "(.*)_(mean|sd)$") %>%
        mutate(across(where(is.numeric), ~ round(.x, 4))) %>%
        head(6) %>% as.data.frame())
cat("\n")

# ------------------------------------------------------------------------------
# 4. 存盘
# ------------------------------------------------------------------------------
saveRDS(list(split = split, train = train, test = test,
             folds = folds, n_folds = N_FOLDS,
             rec = rec, n_pred = ncol(baked) - 1),
        "results/split.rds")

cat("Saved: results/split.rds\n")
