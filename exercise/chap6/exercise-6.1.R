# 习题 6.1：树高与胸径的简单线性回归

data("larch", package = "forestat")
stopifnot(all(c("D", "H") %in% names(larch)))
d <- subset(larch, complete.cases(D, H))

# （1）D 为胸径（cm），H 为树高（m）；核对缺失并绘制散点图。
cat("有效样本量：", nrow(d), "；D/H 缺失行：", sum(!complete.cases(larch[c("D", "H")])), "\n")
print(summary(d[c("D", "H")]))

if (!interactive()) grDevices::cairo_pdf(tempfile("exercise-6.1-", fileext = ".pdf"), width = 9, height = 6)
par(mfrow = c(1, 2))
plot(D ~ H, data = d, pch = 16, cex = 0.45, col = rgb(0, 0, 0, 0.25), xlab = "树高 H（m）", ylab = "胸径 D（cm）", main = "胸径-树高散点图")

# （2）拟合 D ~ H 并报告方程、区间、R²和斜率 p 值。
model <- lm(D ~ H, data = d)
model_summary <- summary(model)
print(model_summary)
print(confint(model))
cat("R²：", model_summary$r.squared, "；调整 R²：", model_summary$adj.r.squared, "\n")

# （3）残差诊断，并把斜率解释为线性关联。
plot(model, which = 1)
plot(model, which = 2)
if (!interactive()) dev.off()
cat("斜率描述 H 与 D 的条件线性关联，不证明树高变化导致胸径变化。\n")
