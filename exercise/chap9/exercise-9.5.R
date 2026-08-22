# 习题 9.5：线性、多项式、样条和 GAM 的方法比较

library(splines)
library(mgcv)
library(ggplot2)
data("Loblolly", package = "datasets")
dat <- Loblolly

# （1）普通线性模型只有恒定斜率，不能表示弯曲、平台或局部变化。
cat("示例使用 Loblolly：响应为树高 height，预测变量为年龄 age。\n")

# （2）按 Seed 分组五折验证，四个模型使用相同样本和外层折。
set.seed(123)
trees <- sample(levels(dat$Seed))
tree_fold <- setNames(rep(1:5, length.out = length(trees)), trees)
fold_id <- unname(tree_fold[as.character(dat$Seed)])

model_names <- c("linear", "polynomial", "spline", "GAM")
cv_predictions <- lapply(1:5, function(fold) {
  train <- dat[fold_id != fold, ]
  test <- dat[fold_id == fold, ]
  models <- list(
    linear = lm(height ~ age, data = train),
    polynomial = lm(height ~ poly(age, 3), data = train),
    spline = lm(height ~ ns(age, df = 4), data = train),
    GAM = gam(height ~ s(age, k = 6), data = train, method = "REML")
  )
  data.frame(
    observed = test$height,
    as.data.frame(lapply(models, predict, newdata = test), check.names = FALSE)
  )
})
cv_predictions <- do.call(rbind, cv_predictions)
metrics <- function(predicted) c(
  RMSE = sqrt(mean((cv_predictions$observed - predicted)^2)),
  MAE = mean(abs(cv_predictions$observed - predicted)),
  R2 = 1 - sum((cv_predictions$observed - predicted)^2) /
    sum((cv_predictions$observed - mean(cv_predictions$observed))^2)
)
print(do.call(rbind, lapply(cv_predictions[model_names], metrics)))

full_models <- list(
  linear = lm(height ~ age, data = dat),
  polynomial = lm(height ~ poly(age, 3), data = dat),
  spline = lm(height ~ ns(age, df = 4), data = dat),
  GAM = gam(height ~ s(age, k = 6), data = dat, method = "REML")
)
grid <- data.frame(age = seq(min(dat$age), max(dat$age), length.out = 200))
curves <- do.call(rbind, lapply(names(full_models), function(name) {
  data.frame(age = grid$age, height = as.numeric(predict(full_models[[name]], newdata = grid)), model = name)
}))

if (!interactive()) grDevices::cairo_pdf(tempfile("exercise-9.5-", fileext = ".pdf"), width = 9, height = 6)
print(
  ggplot(dat, aes(age, height)) +
    geom_point(alpha = 0.35) +
    geom_line(data = curves, aes(colour = model), linewidth = 1) +
    labs(title = "四类生长曲线比较", x = "年龄", y = "树高") +
    theme_minimal()
)
if (!interactive()) dev.off()

# （3）总结方法边界。
cat(
  "线性模型最易解释和外推；多项式全局耦合且边界可能不稳；\n",
  "回归样条局部灵活；GAM 通过惩罚自动控制复杂度，但解释依赖平滑项。\n",
  "任何模型都应报告分组验证误差和可接受的外推边界。\n",
  sep = ""
)
