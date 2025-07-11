library(ggplot2)
library(dplyr)
library(agricolae)
# 加载 CO2 数据集
data(CO2)
# （1）方差分析：Type 对 uptake 的影响
anova_model <- aov(uptake ~ factor(Type), data = CO2)
summary(anova_model)
# （2）事后 Tukey 检验
tukey_result <- HSD.test(anova_model, "factor(Type)", group = TRUE)
print(tukey_result)
# （3）绘制箱线图
ggplot(CO2, aes(x = Type, y = uptake, fill = Type)) +
  geom_boxplot() +
  labs(title = "不同植物类型下的光合作用速率分布",
       x = "植物类型", y = "光合作用速率（uptake）") +
  theme_minimal()
