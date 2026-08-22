# 习题 9.4：冠幅-胸径候选曲线及按 PLOT 分组验证

library(mgcv)
library(splines)
library(ggplot2)
data("larch", package = "forestat")
d <- subset(larch, complete.cases(D, CW, PLOT))
d$PLOT <- factor(d$PLOT)

# （1）D 为胸径（cm），CW 为冠幅（m）；整块 PLOT 放入同一外层折。
print(summary(d[c("D", "CW")]))
set.seed(123)
plots <- sample(levels(d$PLOT))
plot_fold <- setNames(rep(1:5, length.out = length(plots)), plots)
fold_id <- unname(plot_fold[as.character(d$PLOT)])
stopifnot(all(tapply(fold_id, d$PLOT, function(x) length(unique(x))) == 1L))

# （2）复杂度预先固定；所有候选模型只在各训练折拟合。
model_names <- c("quadratic", "cubic", "spline", "loess", "GAM")
fold_predictions <- lapply(1:5, function(fold) {
  train <- d[fold_id != fold, ]
  test <- d[fold_id == fold, ]
  models <- list(
    quadratic = lm(CW ~ poly(D, 2), data = train),
    cubic = lm(CW ~ poly(D, 3), data = train),
    spline = lm(CW ~ ns(D, df = 4), data = train),
    loess = loess(CW ~ D, data = train, span = 0.75, control = loess.control(surface = "direct")),
    GAM = gam(CW ~ s(D, k = 6), data = train, method = "REML")
  )
  predicted <- lapply(models, predict, newdata = test)
  stopifnot(all(vapply(predicted, function(x) all(is.finite(x)), logical(1))))
  data.frame(observed = test$CW, as.data.frame(predicted, check.names = FALSE))
})
fold_predictions <- do.call(rbind, fold_predictions)

# （3）统一报告样本外 RMSE、MAE 和 R²。
metrics <- function(predicted) c(
  RMSE = sqrt(mean((fold_predictions$observed - predicted)^2)),
  MAE = mean(abs(fold_predictions$observed - predicted)),
  R2 = 1 - sum((fold_predictions$observed - predicted)^2) /
    sum((fold_predictions$observed - mean(fold_predictions$observed))^2)
)
comparison <- do.call(rbind, lapply(fold_predictions[model_names], metrics))
print(comparison)

# 同图展示使用全数据拟合的描述性曲线；验证结果仍来自上面的分组外层折。
full_models <- list(
  quadratic = lm(CW ~ poly(D, 2), data = d),
  cubic = lm(CW ~ poly(D, 3), data = d),
  spline = lm(CW ~ ns(D, df = 4), data = d),
  loess = loess(CW ~ D, data = d, span = 0.75, control = loess.control(surface = "direct")),
  GAM = gam(CW ~ s(D, k = 6), data = d, method = "REML")
)
grid <- data.frame(D = seq(min(d$D), max(d$D), length.out = 300))
curve_data <- do.call(rbind, lapply(names(full_models), function(name) {
  data.frame(D = grid$D, CW = as.numeric(predict(full_models[[name]], newdata = grid)), model = name)
}))
if (!interactive()) grDevices::cairo_pdf(tempfile("exercise-9.4-", fileext = ".pdf"), width = 9, height = 6)
print(
  ggplot(d, aes(D, CW)) +
    geom_point(alpha = 0.15) +
    geom_line(data = curve_data, aes(colour = model), linewidth = 1) +
    labs(title = "冠幅-胸径候选曲线", x = "胸径 D（cm）", y = "冠幅 CW（m）") +
    theme_minimal()
)
if (!interactive()) dev.off()

# （4）综合误差、残差、复杂度和外推行为选择。
cat("优先验证误差稳定、残差可接受、边界行为合理的较简单模型；所有曲线均不应无依据外推。\n")
