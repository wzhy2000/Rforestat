# 习题 7.1：树高与冠幅的 Gamma 广义线性模型

data("larch", package = "forestat")
d <- subset(larch, complete.cases(CW, H))

# （1）检查严格正值、偏态以及均值-方差关系。
stopifnot(all(d$CW > 0))
skewness <- mean((d$CW - mean(d$CW))^3) / sd(d$CW)^3
height_bin <- cut(d$H, breaks = quantile(d$H, seq(0, 1, 0.2)), include.lowest = TRUE, unique = TRUE)
mean_variance <- aggregate(d$CW, list(height_bin = height_bin), function(x) c(mean = mean(x), variance = var(x)))
print(summary(d[c("CW", "H")]))
cat("CW 偏度：", skewness, "\n")
print(mean_variance)

# （2）拟合对数连接 Gamma GLM，并报告系数和 Wald 区间。
model <- glm(CW ~ H, family = Gamma(link = "log"), data = d)
print(summary(model))
print(confint.default(model))
cat("H 每增加 1 m，条件均值的乘法变化：", exp(coef(model)["H"]), "\n")

# （3）在响应尺度绘制拟合曲线。
grid <- data.frame(H = seq(min(d$H), max(d$H), length.out = 200))
grid$fit <- predict(model, newdata = grid, type = "response")

if (!interactive()) grDevices::cairo_pdf(tempfile("exercise-7.1-", fileext = ".pdf"), width = 9, height = 5)
par(mfrow = c(1, 2))
plot(CW ~ H, data = d, pch = 16, cex = 0.35, col = rgb(0, 0, 0, 0.2), xlab = "树高 H（m）", ylab = "冠幅 CW（m）")
lines(grid$H, grid$fit, col = "red", lwd = 2)

# （4）报告偏差/AIC并检查偏差残差。
plot(fitted(model), residuals(model, type = "deviance"), pch = 16, cex = 0.35, col = rgb(0, 0, 0, 0.2), xlab = "拟合值", ylab = "偏差残差")
abline(h = 0, lty = 2, col = "red")
if (!interactive()) dev.off()
print(c(residual_deviance = deviance(model), df = df.residual(model), AIC = AIC(model)))
cat("模型描述横断面统计关联，不表示树高导致冠幅生长。\n")
