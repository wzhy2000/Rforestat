# 加载所需包
library(dplyr)
library(ggplot2)
data(Loblolly)
# 查看数据结构
str(Loblolly)
# （1）筛选 age > 10 且 Seed == "301"，并按 Seed 计算平均树高
filtered <- Loblolly %>% 
  filter(age > 10, Seed == "301")
# 输出筛选结果
print(filtered)
# 按 Seed 计算平均树高
avg_height <- filtered %>%
  group_by(Seed) %>%
  summarise(平均树高 = mean(height))
print(avg_height)
# （2）绘制不同 Seed 的树高分布图
# 曲线密度图
ggplot(Loblolly, aes(x = height, color = Seed, fill = Seed)) +
  geom_density(alpha = 0.3) +
  theme_minimal() +
  labs(title = "不同Seed的树高密度分布图",
       x = "树高（feet）", y = "密度") +
  theme(plot.title = element_text(hjust = 0.5))
# 小提琴图
ggplot(Loblolly, aes(x = Seed, y = height, fill = Seed)) +
  geom_violin(trim = FALSE) +
  theme_classic() +
  labs(title = "不同Seed的树高小提琴图",
       x = "Seed（家系）", y = "树高（feet）") +
  theme(plot.title = element_text(hjust = 0.5))

