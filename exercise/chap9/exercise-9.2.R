# 加载必要的包
library(ggplot2)
library(mgcv)
library(dplyr)

# 加载CO2数据集
data(CO2)
# LOESS 拟合图
ggplot(CO2, aes(x = conc, y = uptake, color = Type)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE, span = 0.75) +
  theme_minimal() +
  labs(title = "不同植物来源 Type 的 CO₂ 响应趋势",
       x = "CO₂ 浓度 (ppm)", y = "光合作用速率 (umol/m²/s)")

# 可视化：按 Type × Treatment 分面绘图
ggplot(CO2, aes(x = conc, y = uptake)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 5), se = FALSE, color = "blue") +
  facet_grid(Type ~ Treatment) +
  theme_minimal() +
  labs(title = "CO₂ 响应趋势（按 Type × Treatment 分组）",
       x = "CO₂ 浓度 (ppm)", y = "光合作用速率 (umol/m²/s)")

# 简单探索“响应拐点”可选方法：创建新浓度序列 + 预测值
newdata <- expand.grid(
  conc = seq(min(CO2$conc), max(CO2$conc), length.out = 200),
  Type = levels(CO2$Type),
  Treatment = levels(CO2$Treatment)
)

### 提取 GAM 拟合值并目测大致拐点

# 拟合 GAM 模型（含交互）
model_gam <- gam(uptake ~ s(conc, by = interaction(Type, Treatment), k = 4) + Type + Treatment,
                 data = CO2)
# 模型摘要
summary(model_gam)
#提取预测值
newdata$uptake_pred <- predict(model_gam, newdata = newdata)

# 可视化预测趋势
ggplot(newdata, aes(x = conc, y = uptake_pred, color = Type)) +
  geom_line(size = 1) +
  facet_grid(Type ~ Treatment) +
  theme_minimal() +
  labs(title = "预测的 CO₂ 响应趋势（可辅助寻找拐点）",
       y = "预测光合速率", x = "CO₂ 浓度 (ppm)")
