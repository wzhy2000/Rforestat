# 习题 8.3：Loblolly 的随机截距和随机斜率模型

library(lme4)
data("Loblolly", package = "datasets")
d <- transform(Loblolly, age_centered = age - mean(age))

# （1）每棵树 6 次观测，绘制个体生长轨迹。
print(table(d$Seed))
stopifnot(all(table(d$Seed) == 6L))
if (!interactive()) grDevices::cairo_pdf(tempfile("exercise-8.3-", fileext = ".pdf"), width = 9, height = 5)
plot(height ~ age, data = d, type = "n", xlab = "年龄", ylab = "树高", main = "14 棵树的生长轨迹")
for (tree in levels(d$Seed)) {
  z <- d[d$Seed == tree, ]
  lines(z$age, z$height, col = adjustcolor(as.integer(tree), alpha.f = 0.55))
  points(z$age, z$height, col = as.integer(tree), pch = 16, cex = 0.5)
}

# （2）随机截距模型。
random_intercept <- lmer(height ~ age_centered + (1 | Seed), data = d, REML = FALSE)
print(summary(random_intercept))
print(VarCorr(random_intercept))

# （3）考察随机斜率，比较 AIC、方差和奇异性。
random_slope <- lmer(height ~ age_centered + (age_centered | Seed), data = d, REML = FALSE)
print(AIC(random_intercept, random_slope))
print(VarCorr(random_slope))
cat("随机斜率模型是否奇异：", isSingular(random_slope), "\n")
plot(fitted(random_slope), resid(random_slope), pch = 16, cex = 0.5, xlab = "拟合值", ylab = "残差")
abline(h = 0, lty = 2, col = "red")
if (!interactive()) dev.off()

# （4）Seed 是树木标识，不是家系遗传设计。
cat("固定年龄斜率描述总体趋势；随机效应描述树木间差异，不能称为遗传方差。\n")
