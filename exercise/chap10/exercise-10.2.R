# 习题 10.2：Loblolly 线性、Logistic 与 Gompertz 生长模型

library(minpack.lm)
library(ggplot2)
data("Loblolly", package = "datasets")
d <- Loblolly

# （1）核对整树重复观测并绘制个体轨迹。
print(table(d$Seed))
print(sort(unique(d$age)))
stopifnot(all(table(d$Seed) == 6L))

# （2）拟合三个候选模型并记录起始值与收敛。
linear_model <- lm(height ~ age, data = d)
logistic_start <- list(Asym = 62, xmid = 12, scal = 4)
gompertz_start <- list(Asym = 65, b = 4, c = 0.14)
logistic_model <- nlsLM(
  height ~ Asym / (1 + exp((xmid - age) / scal)),
  data = d, start = logistic_start,
  lower = c(Asym = 0, xmid = -Inf, scal = 0)
)
gompertz_model <- nlsLM(
  height ~ Asym * exp(-b * exp(-c * age)),
  data = d, start = gompertz_start,
  lower = c(Asym = 0, b = 0, c = 0)
)
print(summary(linear_model))
print(summary(logistic_model))
print(summary(gompertz_model))

calc_r2 <- function(observed, predicted) {
  1 - sum((observed - predicted)^2) / sum((observed - mean(observed))^2)
}
sample_fit <- data.frame(
  model = c("linear", "Logistic", "Gompertz"),
  AIC = c(AIC(linear_model), AIC(logistic_model), AIC(gompertz_model)),
  R2 = c(
    calc_r2(d$height, predict(linear_model)),
    calc_r2(d$height, predict(logistic_model)),
    calc_r2(d$height, predict(gompertz_model))
  )
)
print(sample_fit)

# （3）五折外层按 Seed 整树分组，避免同树观测泄漏。
set.seed(123)
trees <- sample(levels(d$Seed))
tree_fold <- setNames(rep(1:5, length.out = length(trees)), trees)
fold_id <- unname(tree_fold[as.character(d$Seed)])
cv <- do.call(rbind, lapply(1:5, function(fold) {
  train <- d[fold_id != fold, ]
  test <- d[fold_id == fold, ]
  ml <- lm(height ~ age, data = train)
  ms <- nlsLM(height ~ Asym / (1 + exp((xmid - age) / scal)), data = train, start = logistic_start, lower = c(0, -Inf, 0))
  mg <- nlsLM(height ~ Asym * exp(-b * exp(-c * age)), data = train, start = gompertz_start, lower = c(0, 0, 0))
  data.frame(
    observed = test$height,
    linear = predict(ml, newdata = test),
    Logistic = predict(ms, newdata = test),
    Gompertz = predict(mg, newdata = test)
  )
}))
metrics <- function(predicted) c(
  RMSE = sqrt(mean((cv$observed - predicted)^2)),
  MAE = mean(abs(cv$observed - predicted)),
  R2 = calc_r2(cv$observed, predicted)
)
print(rbind(linear = metrics(cv$linear), Logistic = metrics(cv$Logistic), Gompertz = metrics(cv$Gompertz)))

# （4）曲线只在 3--25 岁观测范围内展示。
grid <- data.frame(age = seq(min(d$age), max(d$age), length.out = 200))
curves <- rbind(
  data.frame(grid, height = predict(linear_model, newdata = grid), model = "linear"),
  data.frame(grid, height = predict(logistic_model, newdata = grid), model = "Logistic"),
  data.frame(grid, height = predict(gompertz_model, newdata = grid), model = "Gompertz")
)
if (!interactive()) grDevices::cairo_pdf(tempfile("exercise-10.2-", fileext = ".pdf"), width = 9, height = 6)
print(
  ggplot(d, aes(age, height, group = Seed)) +
    geom_line(alpha = 0.25) + geom_point(alpha = 0.4) +
    geom_line(data = curves, aes(colour = model, group = model), linewidth = 1.1) +
    labs(title = "Loblolly 生长曲线比较", x = "年龄", y = "树高") + theme_minimal()
)
if (!interactive()) dev.off()
cat("观测年龄仅为 3--25 年，Asym 不能解释为完整生命周期真实最大树高。\n")
