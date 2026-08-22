# 习题 14.2：Loblolly 未见新树预测的径向基核 SVR

library(kernlab)
data("Loblolly", package = "datasets")
d <- Loblolly
d$Seed <- factor(d$Seed)

group_folds <- function(group, v, seed) {
  set.seed(seed)
  groups <- sample(unique(as.character(group)))
  assignment <- setNames(rep(seq_len(v), length.out = length(groups)), groups)
  unname(assignment[as.character(group)])
}
scale_train_test <- function(train, test) {
  center <- mean(train$age); scale_value <- sd(train$age)
  list(
    train = matrix((train$age - center) / scale_value, ncol = 1),
    test = matrix((test$age - center) / scale_value, ncol = 1)
  )
}
metrics <- function(observed, predicted) c(
  RMSE = sqrt(mean((observed - predicted)^2)), MAE = mean(abs(observed - predicted)),
  R2 = 1 - sum((observed - predicted)^2) / sum((observed - mean(observed))^2)
)

grid <- expand.grid(C = c(0.1, 1, 10), sigma = c(0.01, 0.1, 1))
outer_fold <- group_folds(d$Seed, 7L, 123)
predictions <- list(); choices <- list()
for (fold in 1:7) {
  training <- droplevels(d[outer_fold != fold, ])
  testing <- d[outer_fold == fold, ]
  inner_fold <- group_folds(training$Seed, 3L, 1000 + fold)
  score <- numeric(nrow(grid))
  for (g in seq_len(nrow(grid))) {
    errors <- numeric(3L)
    for (inner in 1:3) {
      inner_train <- training[inner_fold != inner, ]
      inner_test <- training[inner_fold == inner, ]
      scaled <- scale_train_test(inner_train, inner_test)
      fit <- ksvm(
        x = scaled$train, y = inner_train$height, type = "eps-svr",
        kernel = "rbfdot", kpar = list(sigma = grid$sigma[g]),
        C = grid$C[g], scaled = FALSE
      )
      errors[inner] <- sqrt(mean((inner_test$height - predict(fit, scaled$test))^2))
    }
    score[g] <- mean(errors)
  }
  best <- grid[which.min(score), ]
  scaled <- scale_train_test(training, testing)
  fit <- ksvm(
    x = scaled$train, y = training$height, type = "eps-svr",
    kernel = "rbfdot", kpar = list(sigma = best$sigma), C = best$C, scaled = FALSE
  )
  choices[[fold]] <- data.frame(fold, C = best$C, sigma = best$sigma, support_vectors = nSV(fit), inner_RMSE = min(score))
  predictions[[fold]] <- data.frame(fold, Seed = testing$Seed, age = testing$age, observed = testing$height, predicted = as.numeric(predict(fit, scaled$test)))
}
choices <- do.call(rbind, choices); predictions <- do.call(rbind, predictions)
print(choices, row.names = FALSE)
print(metrics(predictions$observed, predictions$predicted))
print(do.call(rbind, lapply(split(predictions, predictions$fold), function(z) metrics(z$observed, z$predicted))))

if (!interactive()) grDevices::cairo_pdf(tempfile("exercise-14.2-", fileext = ".pdf"), width = 9, height = 6)
plot(predictions$age, predictions$observed, pch = 16, col = as.integer(predictions$Seed), xlab = "年龄", ylab = "树高")
points(predictions$age, predictions$predicted, pch = 1, col = as.integer(predictions$Seed))
legend("topleft", c("观测", "外层预测"), pch = c(16, 1))
if (!interactive()) dev.off()
cat("Seed 只用于整树留出，不作为特征；14 棵树很少，且 3--25 岁以外的 SVR 外推没有保障。\n")
