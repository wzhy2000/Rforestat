# 习题 8.1：样地随机截距的线性混合效应模型

library(lme4)
library(performance)
data("larch", package = "forestat")
d <- subset(larch, complete.cases(PLOT, H, CW, D))
d$PLOT <- factor(d$PLOT)

# （1）核对分组样本量、缺失值和变量范围。
print(summary(table(d$PLOT)))
print(summary(d[c("H", "CW", "D")]))
cat("H/CW/D 缺失行：", sum(!complete.cases(larch[c("H", "CW", "D")])), "\n")

# （2）固定效应为 CW、D，样地为随机截距。
model <- lmer(H ~ CW + D + (1 | PLOT), data = d, REML = TRUE)
print(summary(model))

# （3）报告固定效应区间、方差分量和 ICC。
print(confint(model, parm = "beta_", method = "Wald"))
variance <- as.data.frame(VarCorr(model))
print(variance)
plot_variance <- variance$vcov[variance$grp == "PLOT"]
residual_variance <- sigma(model)^2
icc <- plot_variance / (plot_variance + residual_variance)
cat("ICC：", icc, "\n")
print(r2_nakagawa(model))

# （4）检查条件残差与样地随机效应，不作因果解释。
if (!interactive()) grDevices::cairo_pdf(tempfile("exercise-8.1-", fileext = ".pdf"), width = 9, height = 5)
par(mfrow = c(1, 2))
plot(fitted(model), resid(model), pch = 16, cex = 0.35, col = rgb(0, 0, 0, 0.2), xlab = "拟合值", ylab = "条件残差")
abline(h = 0, lty = 2, col = "red")
qqnorm(ranef(model)$PLOT[[1]], main = "样地随机截距 Q-Q 图")
qqline(ranef(model)$PLOT[[1]], col = "red")
if (!interactive()) dev.off()
cat("固定效应表示控制另一尺寸变量后的条件关联；样地随机效应描述未观测异质性，不是因果环境效应。\n")
