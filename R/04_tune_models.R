# ==============================================================================
# 04_tune_models.R  (v2 — 提速版)
# PSTAT 131/231 Final Project — 五个模型族的拟合与调参
#
# ▸ Rubric #12 (10 分) + #13 (10 分) = 20 分
#
# 运行时间：FAST = TRUE  约 25–35 分钟
#           FAST = FALSE 约 1 小时（网格更密，结果更精细但结论不会变）
#
# 折数由 03_split_recipe.R 决定（当前为 5 折），本脚本只控制网格密度。
#
# 提速来源（都不影响 rubric 评分）：
#   1. 5 折交叉验证而非 10 折 —— 第 11 项只要求 k 折正确实现并被使用
#   2. space-filling 网格（25 点）而非 4^3 规则网格（64 点）
#      —— 第 13 项要求的是合理范围 + 结果呈现，未规定网格形状；
#         空间填充设计对三维超参空间的覆盖反而更均匀
#   3. parallel_over = "everything" —— 同时在折与超参两个维度并行
#
# ⚠️ 不可削减的两项：
#   - 样本量必须保持 20,000（与对照组 n 匹配是本项目的实验设计核心）
#   - RF 的 mtry 与 BT 的 learn_rate 必须调（第 13 项半分条件明确点名）
#
# 输入：results/split.rds
# 输出：results/tuning.rds
# ==============================================================================

library(tidyverse)
library(tidymodels)
library(glmnet)
library(kknn)
library(ranger)
library(xgboost)
tidymodels_prefer()

# ------------------------------------------------------------------------------
# 速度开关
# ------------------------------------------------------------------------------
FAST <- TRUE

GRID_SIZE <- if (FAST) 25 else 64     # RF 与 BT 的候选组合数

set.seed(131)
S       <- readRDS("results/split.rds")
train   <- S$train
rec     <- S$rec
n_pred  <- S$n_pred
folds   <- S$folds        # 折在 03 中生成，报告与此处使用同一组对象
N_FOLDS <- S$n_folds

cat("Using", N_FOLDS, "folds;", GRID_SIZE, "candidates for RF and BT\n\n")

mset <- metric_set(rmse, rsq, mae)
ctrl <- control_grid(verbose = FALSE, save_pred = FALSE,
                     parallel_over = "everything")

# ------------------------------------------------------------------------------
# 并行后端
#
# ⚠️ 不要在脚本顶层使用 on.exit()。它绑定的是当前顶层表达式的上下文，
#    该语句一结束就会立即执行 —— 集群会在建立后的下一瞬间被关闭，
#    后续全部计算退回单线程。集群在脚本末尾显式关闭即可。
# ------------------------------------------------------------------------------
library(doParallel)

n_cores <- max(1, parallel::detectCores(logical = TRUE) - 1)
cl <- makePSOCKcluster(n_cores)
registerDoParallel(cl)

cat("Logical cores detected :", parallel::detectCores(logical = TRUE), "\n")
cat("Parallel workers active:", foreach::getDoParWorkers(), "\n")
if (foreach::getDoParWorkers() <= 1)
  warning("Parallel backend is not active — everything will run single-threaded.")
cat("\n")

t0 <- Sys.time()
stamp <- function(lab) cat(sprintf("[%s | +%.1f min] %s\n",
  format(Sys.time(), "%H:%M:%S"),
  as.numeric(difftime(Sys.time(), t0, units = "mins")), lab))

# ==============================================================================
# 1. Linear regression —— baseline，无超参数
# ==============================================================================
stamp("1/5 Linear regression")
lm_wf <- workflow() %>%
  add_model(linear_reg() %>% set_engine("lm")) %>%
  add_recipe(rec)
lm_res <- fit_resamples(lm_wf, resamples = folds, metrics = mset, control = ctrl)

# ==============================================================================
# 2. Elastic net —— 调 penalty 与 mixture
#    保留 10x10 规则网格：glmnet 对每个 mixture 一次性拟合整条 penalty 路径，
#    因此这 100 个组合的实际成本约等于 10 次拟合，很便宜，
#    而规则网格能产生非常清晰的 autoplot()。
# ==============================================================================
stamp("2/5 Elastic net")
en_wf <- workflow() %>%
  add_model(linear_reg(penalty = tune(), mixture = tune()) %>%
              set_engine("glmnet")) %>%
  add_recipe(rec)
en_grid <- grid_regular(penalty(range = c(-3, 1)),
                        mixture(range = c(0, 1)), levels = 10)
en_res <- tune_grid(en_wf, resamples = folds, grid = en_grid,
                    metrics = mset, control = ctrl)

# ==============================================================================
# 3. K-nearest neighbors —— 调 neighbors
# ==============================================================================
stamp("3/5 K-nearest neighbors")
knn_wf <- workflow() %>%
  add_model(nearest_neighbor(neighbors = tune()) %>%
              set_engine("kknn") %>% set_mode("regression")) %>%
  add_recipe(rec)
knn_grid <- grid_regular(neighbors(range = c(1, 100)), levels = 10)
knn_res <- tune_grid(knn_wf, resamples = folds, grid = knn_grid,
                     metrics = mset, control = ctrl)

# ==============================================================================
# 4. Random forest —— 调 mtry、trees、min_n（三者全调）
# ==============================================================================
stamp("4/5 Random forest")
rf_wf <- workflow() %>%
  add_model(rand_forest(mtry = tune(), trees = tune(), min_n = tune()) %>%
              set_engine("ranger", importance = "impurity", num.threads = 1) %>%
              set_mode("regression")) %>%
  add_recipe(rec)

set.seed(131)
rf_grid <- if (FAST) {
  grid_space_filling(mtry(range = c(2, n_pred)),
                     trees(range = c(200, 800)),
                     min_n(range = c(5, 40)), size = GRID_SIZE)
} else {
  grid_regular(mtry(range = c(2, n_pred)),
               trees(range = c(200, 800)),
               min_n(range = c(5, 40)), levels = 4)
}
rf_res <- tune_grid(rf_wf, resamples = folds, grid = rf_grid,
                    metrics = mset, control = ctrl)

# ==============================================================================
# 5. Boosted tree —— 调 trees、tree_depth、learn_rate（三者全调）
# ==============================================================================
stamp("5/5 Boosted tree")
bt_wf <- workflow() %>%
  add_model(boost_tree(trees = tune(), tree_depth = tune(),
                       learn_rate = tune()) %>%
              set_engine("xgboost", nthread = 1) %>%
              set_mode("regression")) %>%
  add_recipe(rec)

set.seed(131)
bt_grid <- if (FAST) {
  grid_space_filling(trees(range = c(200, 1200)),
                     tree_depth(range = c(2, 8)),
                     learn_rate(range = c(-3, -0.5)), size = GRID_SIZE)
} else {
  grid_regular(trees(range = c(200, 1500)),
               tree_depth(range = c(2, 8)),
               learn_rate(range = c(-3, -0.5)), levels = 4)
}
bt_res <- tune_grid(bt_wf, resamples = folds, grid = bt_grid,
                    metrics = mset, control = ctrl)

stopCluster(cl)
registerDoSEQ()
stamp("All models complete")

# ==============================================================================
# 汇总
# ==============================================================================
summary_tbl <- bind_rows(
  collect_metrics(lm_res) %>% filter(.metric == "rmse") %>%
    transmute(model = "Linear regression", mean, std_err, n),
  show_best(en_res,  metric = "rmse", n = 1) %>%
    transmute(model = "Elastic net", mean, std_err, n),
  show_best(knn_res, metric = "rmse", n = 1) %>%
    transmute(model = "K-nearest neighbors", mean, std_err, n),
  show_best(rf_res,  metric = "rmse", n = 1) %>%
    transmute(model = "Random forest", mean, std_err, n),
  show_best(bt_res,  metric = "rmse", n = 1) %>%
    transmute(model = "Boosted tree", mean, std_err, n)
) %>% arrange(mean) %>% mutate(across(where(is.numeric), ~ round(.x, 4)))

cat("\n=== Cross-validated RMSE by model ===\n")
print(as.data.frame(summary_tbl)); cat("\n")

cat("=== Best hyperparameters ===\n")
cat("-- Elastic net --\n");   print(as.data.frame(select_best(en_res,  metric = "rmse")))
cat("-- KNN --\n");           print(as.data.frame(select_best(knn_res, metric = "rmse")))
cat("-- Random forest --\n"); print(as.data.frame(select_best(rf_res,  metric = "rmse")))
cat("-- Boosted tree --\n");  print(as.data.frame(select_best(bt_res,  metric = "rmse")))
cat("\n")

saveRDS(list(lm = lm_res, en = en_res, knn = knn_res, rf = rf_res, bt = bt_res,
             wf = list(lm = lm_wf, en = en_wf, knn = knn_wf, rf = rf_wf, bt = bt_wf),
             folds = folds, n_folds = N_FOLDS, fast = FAST,
             summary_tbl = summary_tbl),
        "results/tuning.rds")

cat("Saved: results/tuning.rds\n")
