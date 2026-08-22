# 习题 4.3：Type 对 uptake 的单因素方差分析

library(ggplot2)
data("CO2", package = "datasets")

# （1）绘制分组图，拟合单因素 ANOVA 并检查残差。
if (!interactive()) grDevices::cairo_pdf(tempfile("exercise-4.3-", fileext = ".pdf"), width = 9, height = 6)
print(
  ggplot(CO2, aes(x = Type, y = uptake, fill = Type)) +
    geom_boxplot(outlier.shape = NA, alpha = 0.5) +
    geom_jitter(width = 0.08, alpha = 0.5) +
    labs(title = "不同植物来源的光合作用速率", x = "植物来源 Type", y = "uptake") +
    theme_minimal()
)

anova_model <- aov(uptake ~ Type, data = CO2)
print(summary(anova_model))
par(mfrow = c(1, 2))
plot(anova_model, which = 1)
plot(anova_model, which = 2)
if (!interactive()) dev.off()

# （2）报告组均值、效应方向和模型系数区间；Type 只有两个水平，不做 Tukey 检验。
group_means <- aggregate(uptake ~ Type, data = CO2, FUN = mean)
print(group_means)
linear_model <- lm(uptake ~ Type, data = CO2)
print(coef(summary(linear_model)))
print(confint(linear_model))
cat("Type 只有两个水平，总体 F 检验已经完成两组比较，无需冗余 Tukey 多重比较。\n")

# （3）普通 ANOVA 忽略同一 Plant 内七次重复测量，只作为教学近似。
print(table(CO2$Plant))
cat("正式推断应使用 Plant 随机效应或适当的重复测量相关结构。\n")
