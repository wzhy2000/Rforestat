# 习题 7.3：Brachy 绝对计数和相对组成的广义线性模型

library(vegan)
library(MASS)
library(ggplot2)
data("mite", package = "vegan")
data("mite.env", package = "vegan")

# （1）构造绝对计数、总计数和相对组成。
Brachy <- mite[[1]]
total <- rowSums(mite)
d <- transform(mite.env, Brachy = Brachy, total = total, proportion = Brachy / total)
stopifnot(all(d$Brachy <= d$total), all(d$total > 0L))
print(summary(d[c("Brachy", "total", "proportion")]))
cat("Brachy 零值数：", sum(d$Brachy == 0), "\n")

# （2）Poisson 计数模型及过度离散检查；明显过度离散时拟合负二项模型。
poisson_model <- glm(Brachy ~ Topo, family = poisson, data = d)
phi_poisson <- sum(residuals(poisson_model, type = "pearson")^2) / df.residual(poisson_model)
cat("Poisson Pearson 离散统计量：", phi_poisson, "\n")
count_model <- if (phi_poisson > 1.5) glm.nb(Brachy ~ Topo, data = d) else poisson_model
print(summary(count_model))
print(confint.default(count_model))

# （3）以总计数为分母拟合二项组成模型，并检查过度离散。
binomial_model <- glm(cbind(Brachy, total - Brachy) ~ Topo, family = binomial, data = d)
phi_binomial <- sum(residuals(binomial_model, type = "pearson")^2) / df.residual(binomial_model)
cat("Binomial Pearson 离散统计量：", phi_binomial, "\n")
composition_model <- if (phi_binomial > 1.5) {
  glm(cbind(Brachy, total - Brachy) ~ Topo, family = quasibinomial, data = d)
} else {
  binomial_model
}
print(summary(composition_model))
print(confint.default(composition_model))

# （4）比较 Topo 分组观测分布，绝对计数与相对组成分别解释。
print(aggregate(cbind(Brachy, proportion, total) ~ Topo, data = d, mean))
if (!interactive()) grDevices::cairo_pdf(tempfile("exercise-7.3-", fileext = ".pdf"), width = 9, height = 5)
par(mfrow = c(1, 2))
boxplot(Brachy ~ Topo, data = d, ylab = "Brachy 计数", main = "绝对计数")
boxplot(proportion ~ Topo, data = d, ylab = "Brachy / total", main = "相对组成")
if (!interactive()) dev.off()
cat("绝对计数和相对组成回答不同问题；效应方向应结合各自区间谨慎解释。\n")
