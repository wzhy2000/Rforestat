# 加载数据和必要包
data(trees)
library(dplyr)
library(ggplot2)

# （1）拟合多个非线性回归模型

# 幂函数模型: Volume = a * Girth^b * Height^c
model_power <- nls(Volume ~ a * Girth^b * Height^c,
                   data = trees,
                   start = list(a = 0.1, b = 1, c = 1))

# 指数模型: Volume = a * exp(b * Girth + c * Height)
model_exp <- nls(Volume ~ a * exp(b * Girth + c * Height),
                 data = trees,
                 start = list(a = 0.1, b = 0.05, c = 0.05))

# （2）比较模型拟合优度（AIC 与 R²）

# 自定义函数计算 R²
calc_r2 <- function(model, data) {
  y <- data$Volume
  y_hat <- predict(model)
  rss <- sum((y - y_hat)^2)
  tss <- sum((y - mean(y))^2)
  r2 <- 1 - rss / tss
  return(r2)
}

# 汇总模型对比
model_comparison <- tibble(
  Model = c("幂函数", "指数模型"),
  AIC = c(AIC(model_power), AIC(model_exp)),
  R_squared = c(calc_r2(model_power, trees), calc_r2(model_exp, trees))
)

print(model_comparison)

# （3）模型参数提取与变量敏感性分析

coef_power <- coef(model_power)
coef_exp <- coef(model_exp)

cat("幂函数模型参数：\n")
print(coef_power)
cat("\n指数模型参数（在指数函数内部的斜率）：\n")
print(coef_exp)

# 哪个变量对产量更敏感？
cat("\n生态意义提示：\n")
if (abs(coef_power["b"]) > abs(coef_power["c"])) {
  cat("- 胸径对材积预测更敏感（指数大于树高）\n")
} else {
  cat("- 树高对材积预测更敏感（指数大于胸径）\n")
}

# 可视化实际 vs 预测（幂函数为例）
ggplot(trees, aes(x = Volume, y = predict(model_power))) +
  geom_point(color = "blue", size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(title = "幂函数模型预测 vs 实际材积",
       x = "实际材积", y = "预测材积") +
  theme_minimal()
