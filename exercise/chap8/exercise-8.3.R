# 加载必要包
library(nlme)
library(ggplot2)

# 查看数据结构
data(Loblolly)
str(Loblolly)

# 确保 Seed 是因子类型（代表家系/样地）
Loblolly$Seed <- as.factor(Loblolly$Seed)

### （1）以 age 为固定效应，Seed 为随机效应，构建线性混合模型
model_lme <- lme(height ~ age, 
                 random = ~ 1 | Seed, 
                 data = Loblolly)

# 查看模型摘要
summary(model_lme)

### （2）分析固定效应显著性和生长趋势

# 固定效应部分（age 的系数）
fixef(model_lme)

# 检验 age 是否显著影响 height
anova(model_lme)

# 可视化拟合效果
ggplot(Loblolly, aes(x = age, y = height, group = Seed, color = Seed)) +
  geom_point() +
  geom_line(aes(y = predict(model_lme)), size = 1) +
  labs(title = "幼树高度随时间变化的线性混合模型",
       x = "年龄（年）", y = "树高（英尺）") +
  theme_minimal()

### （3）解释固定与随机效应（输出随机效应的估计）

# 随机效应（家系差异）
ranef(model_lme)

# 可选：查看方差组成
VarCorr(model_lme)

