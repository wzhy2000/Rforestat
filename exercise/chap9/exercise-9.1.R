# 习题 9.1：物种丰富度的多项式、样条和 GAM 比较

library(vegan)
library(MASS)
library(mgcv)
library(splines)
library(ggplot2)
data("mite", package = "vegan")
data("mite.env", package = "vegan")

# （1）计算计数响应 Richness 并核对预测变量。
d <- transform(mite.env, Richness = specnumber(mite))
d <- d[complete.cases(d[c("Richness", "SubsDens")]), ]
print(with(d, c(min = min(Richness), max = max(Richness), mean = mean(Richness), variance = var(Richness))))
cat("Richness 与 SubsDens 的线性相关：", cor(d$Richness, d$SubsDens), "\n")

# （2）先检查 Poisson 过度离散；三个候选模型统一使用负二项分布。
poisson_check <- glm(Richness ~ SubsDens, family = poisson, data = d)
phi <- sum(residuals(poisson_check, type = "pearson")^2) / df.residual(poisson_check)
cat("Poisson Pearson 离散统计量：", phi, "\n")

quadratic <- glm.nb(Richness ~ poly(SubsDens, 2), data = d)
spline_model <- glm.nb(Richness ~ ns(SubsDens, df = 3), data = d)
gam_model <- gam(Richness ~ s(SubsDens, k = 4), family = nb(), data = d, method = "REML")
print(summary(quadratic))
print(summary(spline_model))
print(summary(gam_model))

predict_interval <- function(model, grid) {
  p <- predict(model, newdata = grid, type = "link", se.fit = TRUE)
  data.frame(
    SubsDens = grid$SubsDens,
    fit = exp(p$fit),
    lower = exp(p$fit - 1.96 * p$se.fit),
    upper = exp(p$fit + 1.96 * p$se.fit)
  )
}

# （3）只在观测范围内使用同一网格绘制响应尺度曲线和区间。
grid <- data.frame(SubsDens = seq(min(d$SubsDens), max(d$SubsDens), length.out = 200))
curves <- rbind(
  transform(predict_interval(quadratic, grid), model = "二次多项式"),
  transform(predict_interval(spline_model, grid), model = "回归样条"),
  transform(predict_interval(gam_model, grid), model = "GAM")
)

if (!interactive()) grDevices::cairo_pdf(tempfile("exercise-9.1-", fileext = ".pdf"), width = 9, height = 6)
print(
  ggplot(d, aes(SubsDens, Richness)) +
    geom_point(alpha = 0.45) +
    geom_ribbon(data = curves, aes(x = SubsDens, ymin = lower, ymax = upper, fill = model), alpha = 0.10, colour = NA, inherit.aes = FALSE) +
    geom_line(data = curves, aes(y = fit, colour = model), linewidth = 1) +
    labs(title = "物种丰富度与土壤密度的非线性关系", y = "物种丰富度") +
    theme_minimal()
)
if (!interactive()) dev.off()

# （4）同为负二项似然时比较 AIC、Pearson 残差和固定五折验证误差。
print(AIC(quadratic, spline_model, gam_model))
set.seed(123)
fold_id <- sample(rep(1:5, length.out = nrow(d)))
cv_predictions <- lapply(1:5, function(fold) {
  train <- d[fold_id != fold, ]
  test <- d[fold_id == fold, ]
  m1 <- glm.nb(Richness ~ poly(SubsDens, 2), data = train)
  m2 <- glm.nb(Richness ~ ns(SubsDens, df = 3), data = train)
  m3 <- gam(Richness ~ s(SubsDens, k = 4), family = nb(), data = train, method = "REML")
  data.frame(
    observed = test$Richness,
    quadratic = predict(m1, newdata = test, type = "response"),
    spline = predict(m2, newdata = test, type = "response"),
    GAM = predict(m3, newdata = test, type = "response")
  )
})
cv_predictions <- do.call(rbind, cv_predictions)
metrics <- function(predicted) c(
  RMSE = sqrt(mean((cv_predictions$observed - predicted)^2)),
  MAE = mean(abs(cv_predictions$observed - predicted))
)
print(rbind(
  quadratic = metrics(cv_predictions$quadratic),
  spline = metrics(cv_predictions$spline),
  GAM = metrics(cv_predictions$GAM)
))
cat("模型选择应综合验证误差、残差和复杂度；若复杂模型无稳定改进，应优先较简单形式。\n")
