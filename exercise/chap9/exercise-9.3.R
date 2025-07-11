library(ggplot2)
library(mgcv)
data(Loblolly)

# loess 局部回归拟合 + 分组趋势图
ggplot(Loblolly, aes(x = age, y = height, color = Seed)) +
  geom_point(size = 2) +
  geom_smooth(method = "loess", se = FALSE, span = 0.75) +
  theme_minimal() +
  labs(title = "不同家系南方松单木树高-树龄局部回归趋势",
       x = "树龄 (年)", y = "树高 (英尺)") +
  theme(legend.position = "bottom")


ggplot(Loblolly, aes(x = age, y = height, color = Seed)) +
  geom_point(size = 2) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 5), se = FALSE) +
  theme_minimal() +
  labs(title = "不同家系南方松单木树高-树龄 GAM 拟合趋势",
       x = "树龄 (年)", y = "树高 (英尺)",
       color = "家系")

# 比较
unique(Loblolly$Seed)
# 创建空表
aic_results <- data.frame(Seed = character(), AIC = numeric())

# 循环拟合 GAM 并且 将各自拟合的AIC存储方便比较
for (seed in unique(Loblolly$Seed)) {
  model <- gam(height ~ s(age, k = 5), data = subset(Loblolly, Seed == seed))
  aic_results <- rbind(aic_results, data.frame(Seed = seed, AIC = AIC(model)))
}

# 查看AIC
aic_results
