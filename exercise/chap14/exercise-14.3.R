# 习题 14.3：螨类 Shannon 多样性的嵌套 KNN 回归

library(vegan)
data("mite", package = "vegan"); data("mite.env", package = "vegan")
d <- transform(mite.env, Shannon = diversity(mite, index = "shannon"))
d <- d[complete.cases(d[c("Shannon", "SubsDens", "WatrCont")]), c("Shannon", "SubsDens", "WatrCont")]
print(summary(d))

standardize <- function(train_x, test_x) {
  center <- colMeans(train_x); scale_value <- apply(train_x, 2, sd)
  list(train = sweep(sweep(train_x, 2, center), 2, scale_value, "/"), test = sweep(sweep(test_x, 2, center), 2, scale_value, "/"))
}
knn_regression <- function(train_x, test_x, train_y, k) {
  k <- min(k, nrow(train_x))
  vapply(seq_len(nrow(test_x)), function(i) {
    distance <- rowSums((train_x - matrix(test_x[i, ], nrow(train_x), ncol(train_x), byrow = TRUE))^2)
    mean(train_y[order(distance)[seq_len(k)]])
  }, numeric(1))
}
metrics <- function(observed, predicted) c(
  RMSE = sqrt(mean((observed - predicted)^2)), MAE = mean(abs(observed - predicted)),
  R2 = 1 - sum((observed - predicted)^2) / sum((observed - mean(observed))^2)
)

k_grid <- seq(3L, 21L, by = 2L)
all_predictions <- list(); choices <- list(); counter <- 0L
for (repeat_id in 1:5) {
  set.seed(100 + repeat_id)
  outer_fold <- sample(rep(1:10, length.out = nrow(d)))
  for (fold in 1:10) {
    counter <- counter + 1L
    training <- d[outer_fold != fold, ]; testing <- d[outer_fold == fold, ]
    set.seed(1000 + counter)
    inner_fold <- sample(rep(1:5, length.out = nrow(training)))
    score <- numeric(length(k_grid))
    for (g in seq_along(k_grid)) {
      errors <- numeric(5L)
      for (inner in 1:5) {
        inner_train <- training[inner_fold != inner, ]; inner_test <- training[inner_fold == inner, ]
        scaled <- standardize(as.matrix(inner_train[c("SubsDens", "WatrCont")]), as.matrix(inner_test[c("SubsDens", "WatrCont")]))
        predicted <- knn_regression(scaled$train, scaled$test, inner_train$Shannon, k_grid[g])
        errors[inner] <- sqrt(mean((inner_test$Shannon - predicted)^2))
      }
      score[g] <- mean(errors)
    }
    best_k <- k_grid[which.min(score)]
    scaled <- standardize(as.matrix(training[c("SubsDens", "WatrCont")]), as.matrix(testing[c("SubsDens", "WatrCont")]))
    predicted <- knn_regression(scaled$train, scaled$test, training$Shannon, best_k)
    choices[[counter]] <- data.frame(repeat_id, fold, k = best_k, inner_RMSE = min(score))
    all_predictions[[counter]] <- data.frame(repeat_id, fold, observed = testing$Shannon, predicted)
  }
}
choices <- do.call(rbind, choices); all_predictions <- do.call(rbind, all_predictions)
print(table(choices$k))
print(metrics(all_predictions$observed, all_predictions$predicted))
quartile <- cut(all_predictions$observed, quantile(all_predictions$observed, c(0, 0.25, 0.75, 1)), include.lowest = TRUE, labels = c("低", "中", "高"))
print(aggregate(abs(all_predictions$observed - all_predictions$predicted), list(Shannon区间 = quartile), function(x) c(MAE = mean(x), SD = sd(x))))

if (!interactive()) grDevices::cairo_pdf(tempfile("exercise-14.3-", fileext = ".pdf"), width = 8, height = 5)
boxplot(abs(all_predictions$observed - all_predictions$predicted) ~ quartile, xlab = "Shannon 区间", ylab = "外层绝对误差")
if (!interactive()) dev.off()
cat("KNN 能表示局部结构，但对尺度、异常值和稀疏区域敏感；mite.env 无坐标，不能声称做了空间阻断。\n")
