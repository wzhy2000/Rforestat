# 习题 4.5：螨虫计数的单样本 t 检验与功效分析

library(vegan)
library(pwr)
data("mite", package = "vegan")
x <- mite[[1]]

# （1）检查零值、偏态和离群值。计数数据的 t 检验仅作均值近似教学。
print(summary(x))
cat("物种：", names(mite)[1], "；样本量：", length(x), "；零值数：", sum(x == 0), "\n", sep = "")
cat("均值：", mean(x), "；标准差：", sd(x), "\n", sep = "")
if (!interactive()) grDevices::cairo_pdf(tempfile("exercise-4.5-", fileext = ".pdf"), width = 8, height = 4)
par(mfrow = c(1, 2))
hist(x, main = "第 1 个物种的计数分布", xlab = "计数", col = "skyblue")
boxplot(x, main = "计数箱线图", ylab = "计数")
if (!interactive()) dev.off()

# （2）检验总体平均计数是否为 5。
one_sample_test <- t.test(x, mu = 5, alternative = "two.sided", conf.level = 0.95)
print(one_sample_test)

# （3）以预先设定的目标均值差 1.5 计算当前样本量下的功效。
target_difference <- 1.5
effect_size <- target_difference / sd(x)
current_power <- pwr.t.test(
  n = length(x), d = effect_size, sig.level = 0.05,
  type = "one.sample", alternative = "two.sided"
)
print(current_power)

# （4）计算目标功效 0.85 所需的最小样本量。
required_n <- pwr.t.test(
  d = effect_size, sig.level = 0.05, power = 0.85,
  type = "one.sample", alternative = "two.sided"
)
print(required_n)
cat("向上取整后的最小样本量：", ceiling(required_n$n), "\n")
cat("标准差由当前偏态计数样本估计，样本量规划应进一步做敏感性分析。\n")
