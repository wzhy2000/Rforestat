# 习题 14.4：相同 PLOT1 外层折上的 GBM 与随机森林

library(forestat)
library(gbm)
library(randomForest)
data("picea", package = "forestat")
picea$AGB <- with(picea, STEM + BRANCH + FOLIAGE + FRUIT)
xvars <- c("LH", "LHCB", "LCW1", "LCW2", "LCW", "CPA")
d <- picea[complete.cases(picea[c("AGB", "PLOT1", xvars)]), c("AGB", "PLOT1", xvars)]
d$PLOT1 <- factor(d$PLOT1)
stopifnot(!any(c("STEM", "BRANCH", "FOLIAGE", "FRUIT") %in% names(d)))

group_folds <- function(group, v, seed) {
  set.seed(seed); groups <- sample(unique(as.character(group)))
  assignment <- setNames(rep(seq_len(v), length.out = length(groups)), groups)
  unname(assignment[as.character(group)])
}
metrics <- function(observed, predicted) c(RMSE = sqrt(mean((observed - predicted)^2)), MAE = mean(abs(observed - predicted)), R2 = 1 - sum((observed - predicted)^2) / sum((observed - mean(observed))^2))
fit_gbm <- function(data, parameter) gbm(
  AGB ~ ., data = data[c("AGB", xvars)], distribution = "gaussian",
  n.trees = parameter$n.trees, interaction.depth = parameter$depth,
  shrinkage = parameter$shrinkage, n.minobsinnode = parameter$min_n,
  bag.fraction = 0.8, train.fraction = 1, verbose = FALSE
)
fit_rf <- function(data, parameter) randomForest(
  x = data[xvars], y = data$AGB, ntree = parameter$ntree,
  mtry = parameter$mtry, nodesize = parameter$nodesize
)

gbm_grid <- data.frame(
  n.trees = c(300, 300, 600, 600, 600, 300), depth = c(1, 3, 1, 3, 2, 2),
  shrinkage = c(0.05, 0.05, 0.03, 0.03, 0.10, 0.10), min_n = c(5, 5, 10, 10, 5, 10)
)
rf_grid <- data.frame(ntree = c(300, 300, 600, 600, 600, 300), mtry = c(2, 4, 2, 4, 6, 6), nodesize = c(5, 5, 15, 15, 5, 15))
outer_fold <- group_folds(d$PLOT1, 5L, 123)
predictions <- list(); importance_rows <- list(); selections <- list()

for (fold in 1:5) {
  training <- droplevels(d[outer_fold != fold, ]); testing <- d[outer_fold == fold, ]
  inner_fold <- group_folds(training$PLOT1, 3L, 1000 + fold)
  tune <- function(grid, fit_function, predict_function) {
    score <- numeric(nrow(grid))
    for (g in seq_len(nrow(grid))) {
      errors <- numeric(3L)
      for (inner in 1:3) {
        tr <- training[inner_fold != inner, ]; va <- training[inner_fold == inner, ]
        model <- fit_function(tr, grid[g, ])
        errors[inner] <- sqrt(mean((va$AGB - predict_function(model, va, grid[g, ]))^2))
      }
      score[g] <- mean(errors)
    }
    list(best = grid[which.min(score), ], rmse = min(score))
  }
  gbm_tuned <- tune(gbm_grid, fit_gbm, function(model, newdata, parameter) predict(model, newdata, n.trees = parameter$n.trees))
  rf_tuned <- tune(rf_grid, fit_rf, function(model, newdata, parameter) predict(model, newdata[xvars]))
  gbm_model <- fit_gbm(training, gbm_tuned$best); rf_model <- fit_rf(training, rf_tuned$best)
  gbm_prediction <- as.numeric(predict(gbm_model, testing, n.trees = gbm_tuned$best$n.trees))
  rf_prediction <- as.numeric(predict(rf_model, testing[xvars]))
  predictions[[fold]] <- rbind(
    data.frame(fold, model = "GBM", observed = testing$AGB, predicted = gbm_prediction),
    data.frame(fold, model = "RF", observed = testing$AGB, predicted = rf_prediction)
  )
  selections[[fold]] <- data.frame(fold, GBM = paste(unlist(gbm_tuned$best), collapse = "/"), RF = paste(unlist(rf_tuned$best), collapse = "/"))
  for (variable in xvars) {
    set.seed(10000 + fold + match(variable, xvars))
    permuted <- testing; permuted[[variable]] <- sample(permuted[[variable]])
    importance_rows[[length(importance_rows) + 1L]] <- data.frame(fold, model = "GBM", variable, delta_RMSE = sqrt(mean((testing$AGB - predict(gbm_model, permuted, n.trees = gbm_tuned$best$n.trees))^2)) - sqrt(mean((testing$AGB - gbm_prediction)^2)))
    importance_rows[[length(importance_rows) + 1L]] <- data.frame(fold, model = "RF", variable, delta_RMSE = sqrt(mean((testing$AGB - predict(rf_model, permuted[xvars]))^2)) - sqrt(mean((testing$AGB - rf_prediction)^2)))
  }
}
predictions <- do.call(rbind, predictions); importance_rows <- do.call(rbind, importance_rows)
print(do.call(rbind, selections), row.names = FALSE)
print(do.call(rbind, lapply(split(predictions, predictions$model), function(z) metrics(z$observed, z$predicted))))
fold_metrics <- do.call(rbind, lapply(split(predictions, list(predictions$fold, predictions$model)), function(z) data.frame(fold = z$fold[1], model = z$model[1], t(metrics(z$observed, z$predicted)))))
print(fold_metrics, row.names = FALSE)
print(aggregate(delta_RMSE ~ model + variable, importance_rows, mean), row.names = FALSE)
cat("两模型共享相同 PLOT1 外层测试行；统一置换重要性表示预测依赖，不作因果解释。\n")
