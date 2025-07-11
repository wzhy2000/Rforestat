# 加载数据
data(CO2)
# 拟合多元线性模型（以 uptake 为因变量，其他为自变量）
model <- lm(uptake ~ conc + Treatment + Type + Plant, data = CO2)
# 查看模型摘要
summary(model)
