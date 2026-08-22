# 习题 14.5：相同 PLOT1 外层折上的三种核 SVR

library(forestat)
library(e1071)
data("picea", package = "forestat")
picea$AGB <- with(picea, STEM + BRANCH + FOLIAGE + FRUIT)
xvars <- c("LH", "LHCB", "LCW1", "LCW2", "LCW", "CPA")
d <- picea[complete.cases(picea[c("AGB", "PLOT1", xvars)]), c("AGB", "PLOT1", xvars)]
d$PLOT1 <- factor(d$PLOT1)

group_folds <- function(group, v, seed) {
  set.seed(seed); groups <- sample(unique(as.character(group)))
  assignment <- setNames(rep(seq_len(v), length.out = length(groups)), groups)
  unname(assignment[as.character(group)])
}
standardize <- function(train, test) {
  center <- colMeans(train[xvars]); scale_value <- apply(train[xvars], 2, sd)
  list(train = sweep(sweep(as.matrix(train[xvars]), 2, center), 2, scale_value, "/"), test = sweep(sweep(as.matrix(test[xvars]), 2, center), 2, scale_value, "/"))
}
metrics <- function(observed, predicted) c(RMSE = sqrt(mean((observed - predicted)^2)), MAE = mean(abs(observed - predicted)), R2 = 1 - sum((observed - predicted)^2) / sum((observed - mean(observed))^2))
fit_svr <- function(x, y, parameter) svm(
  x = x, y = y, type = "eps-regression", kernel = parameter$kernel,
  cost = parameter$cost, gamma = parameter$gamma, degree = parameter$degree,
  scale = FALSE
)

grids <- list(
  Linear = data.frame(kernel = "linear", cost = c(0.1, 1, 10), gamma = 1, degree = 1),
  Polynomial = expand.grid(kernel = "polynomial", cost = c(0.1, 1, 10), gamma = c(0.1, 1), degree = c(2, 3)),
  RBF = expand.grid(kernel = "radial", cost = c(0.1, 1, 10), gamma = c(0.01, 0.1, 1), degree = 1)
)
outer_fold <- group_folds(d$PLOT1, 5L, 123)
predictions <- list(); selections <- list(); counter <- 0L
for (fold in 1:5) {
  training <- droplevels(d[outer_fold != fold, ]); testing <- d[outer_fold == fold, ]
  inner_fold <- group_folds(training$PLOT1, 3L, 1000 + fold)
  for (model_name in names(grids)) {
    grid <- grids[[model_name]]; score <- numeric(nrow(grid))
    for (g in seq_len(nrow(grid))) {
      errors <- numeric(3L)
      for (inner in 1:3) {
        inner_train <- training[inner_fold != inner, ]; inner_test <- training[inner_fold == inner, ]
        scaled <- standardize(inner_train, inner_test)
        model <- fit_svr(scaled$train, inner_train$AGB, grid[g, ])
        errors[inner] <- sqrt(mean((inner_test$AGB - predict(model, scaled$test))^2))
      }
      score[g] <- mean(errors)
    }
    best <- grid[which.min(score), ]; scaled <- standardize(training, testing)
    model <- fit_svr(scaled$train, training$AGB, best)
    predicted <- as.numeric(predict(model, scaled$test))
    counter <- counter + 1L
    selections[[counter]] <- data.frame(fold, model = model_name, cost = best$cost, gamma = best$gamma, degree = best$degree, support_vectors = nrow(model$SV), inner_RMSE = min(score))
    predictions[[counter]] <- data.frame(fold, model = model_name, observed = testing$AGB, predicted)
  }
}
selections <- do.call(rbind, selections); predictions <- do.call(rbind, predictions)
print(selections, row.names = FALSE)
print(do.call(rbind, lapply(split(predictions, predictions$model), function(z) metrics(z$observed, z$predicted))))
fold_metrics <- do.call(rbind, lapply(split(predictions, list(predictions$fold, predictions$model)), function(z) data.frame(fold = z$fold[1], model = z$model[1], t(metrics(z$observed, z$predicted)))))
print(aggregate(cbind(RMSE, MAE, R2) ~ model, fold_metrics, function(x) c(mean = mean(x), sd = sd(x))))
cat("三种核使用完全相同的 PLOT1 外层测试折；线性核外推较平稳，多项式与 RBF 对尺度及参数更敏感。\n")
