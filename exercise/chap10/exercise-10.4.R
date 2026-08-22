# 习题 10.4：黑樱桃材积的幂函数与指数函数

library(minpack.lm)
data("trees", package = "datasets")
d <- trees

# （1）Girth 实为离地 4 ft 6 in 处的树干直径（in），Height 为 ft，Volume 为 ft^3。
cat("样本量：", nrow(d), "\n")
print(summary(d))
if (!interactive()) grDevices::cairo_pdf(tempfile("exercise-10.4-", fileext = ".pdf"), width = 9, height = 5)
par(mfrow = c(1, 2))
plot(d$Girth, d$Volume, xlab = "树干直径（in）", ylab = "材积（ft^3）")
plot(d$Height, d$Volume, xlab = "树高（ft）", ylab = "材积（ft^3）")

# （2）相同加性误差下拟合两个模型。
power_start <- list(a = 0.001, b = 2, c = 1)
exponential_start <- list(a = 2, b = 0.13, c = 0.01)
power_model <- nlsLM(Volume ~ a * Girth^b * Height^c, data = d, start = power_start, lower = c(0, -Inf, -Inf))
exponential_model <- nlsLM(Volume ~ a * exp(b * Girth + c * Height), data = d, start = exponential_start, lower = c(0, -Inf, -Inf))
print(summary(power_model))
print(summary(exponential_model))

# （3）比较 AIC、残差异方差和固定五折验证误差。
print(AIC(power_model, exponential_model))
plot(predict(power_model), residuals(power_model), xlab = "幂函数拟合值", ylab = "残差")
abline(h = 0, lty = 2, col = "red")
if (!interactive()) dev.off()

set.seed(123)
fold_id <- sample(rep(1:5, length.out = nrow(d)))
cv <- do.call(rbind, lapply(1:5, function(fold) {
  train <- d[fold_id != fold, ]
  test <- d[fold_id == fold, ]
  mp <- nlsLM(Volume ~ a * Girth^b * Height^c, data = train, start = power_start, lower = c(0, -Inf, -Inf))
  me <- nlsLM(Volume ~ a * exp(b * Girth + c * Height), data = train, start = exponential_start, lower = c(0, -Inf, -Inf))
  data.frame(observed = test$Volume, power = predict(mp, newdata = test), exponential = predict(me, newdata = test))
}))
metrics <- function(predicted) c(RMSE = sqrt(mean((cv$observed - predicted)^2)), MAE = mean(abs(cv$observed - predicted)))
print(rbind(power = metrics(cv$power), exponential = metrics(cv$exponential)))

# （4）比较代表性取值处局部弹性。
representative <- c(Girth = median(d$Girth), Height = median(d$Height))
cp <- coef(power_model)
ce <- coef(exponential_model)
elasticity <- rbind(
  power = c(Girth = cp["b"], Height = cp["c"]),
  exponential = c(Girth = ce["b"] * representative["Girth"], Height = ce["c"] * representative["Height"])
)
print(representative)
print(elasticity)
