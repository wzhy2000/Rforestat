# 习题 14.1：土壤 CO2 通量的随机森林与 subplot 分组验证

library(fortedata)
library(randomForest)

raw_data <- fd_soil_respiration()
needed <- c("soil_co2_efflux", "soil_temp", "vwc", "subplot_id", "timestamp")
d <- raw_data[complete.cases(raw_data[needed]) & raw_data$soil_co2_efflux > 0, needed]
d$log_efflux <- log(d$soil_co2_efflux)
d$subplot_id <- factor(d$subplot_id)
d <- d[order(d$subplot_id, d$timestamp), ]
cat("样本量：", nrow(d), "；subplot 数：", nlevels(d$subplot_id), "；时间范围：", format(range(d$timestamp)), "\n")

group_folds <- function(group, v, seed) {
  set.seed(seed)
  groups <- sample(unique(as.character(group)))
  assignment <- setNames(rep(seq_len(v), length.out = length(groups)), groups)
  unname(assignment[as.character(group)])
}
metrics <- function(observed, predicted) c(
  RMSE = sqrt(mean((observed - predicted)^2)),
  MAE = mean(abs(observed - predicted)),
  R2 = 1 - sum((observed - predicted)^2) / sum((observed - mean(observed))^2)
)

outer_fold <- group_folds(d$subplot_id, 5L, 123)
tuning_grid <- expand.grid(ntree = c(300L, 600L), mtry = 1:2, nodesize = c(5L, 20L))
outer_predictions <- list(); selected <- list()

for (fold in 1:5) {
  training <- droplevels(d[outer_fold != fold, ])
  testing <- d[outer_fold == fold, ]
  inner_fold <- group_folds(training$subplot_id, 3L, 1000 + fold)
  tuning_rmse <- numeric(nrow(tuning_grid))
  for (g in seq_len(nrow(tuning_grid))) {
    errors <- numeric(3L)
    for (inner in 1:3) {
      inner_train <- training[inner_fold != inner, ]
      inner_test <- training[inner_fold == inner, ]
      fit <- randomForest(
        log_efflux ~ soil_temp + vwc, data = inner_train,
        ntree = tuning_grid$ntree[g], mtry = tuning_grid$mtry[g],
        nodesize = tuning_grid$nodesize[g]
      )
      errors[inner] <- sqrt(mean((inner_test$log_efflux - predict(fit, inner_test))^2))
    }
    tuning_rmse[g] <- mean(errors)
  }
  best <- tuning_grid[which.min(tuning_rmse), ]
  selected[[fold]] <- cbind(fold = fold, best, inner_RMSE = min(tuning_rmse))
  final_model <- randomForest(
    log_efflux ~ soil_temp + vwc, data = training,
    ntree = best$ntree, mtry = best$mtry, nodesize = best$nodesize
  )
  outer_predictions[[fold]] <- data.frame(
    fold, subplot_id = testing$subplot_id, observed = testing$log_efflux,
    predicted = predict(final_model, testing)
  )
}
outer_predictions <- do.call(rbind, outer_predictions)
selected <- do.call(rbind, selected)
print(selected, row.names = FALSE)
print(metrics(outer_predictions$observed, outer_predictions$predicted))
fold_metrics <- do.call(rbind, lapply(split(outer_predictions, outer_predictions$fold), function(z) metrics(z$observed, z$predicted)))
print(fold_metrics)

# 用外层选择次数最多的设置在全数据拟合，仅用于报告置换重要性。
setting <- apply(selected[c("ntree", "mtry", "nodesize")], 1, paste, collapse = "/")
mode_setting <- names(sort(table(setting), decreasing = TRUE))[1]
chosen <- selected[match(mode_setting, setting), ]
final_model <- randomForest(
  log_efflux ~ soil_temp + vwc, data = d,
  ntree = chosen$ntree, mtry = chosen$mtry, nodesize = chosen$nodesize,
  importance = TRUE
)
print(importance(final_model, type = 1))
cat("置换重要性表示预测关联；相关预测变量会分摊重要性，不能据此认定生态因果驱动。\n")
cat("当前分组验证回答新 subplot 外推；若目标是未来预测，应另按 timestamp 作时间阻断。\n")
