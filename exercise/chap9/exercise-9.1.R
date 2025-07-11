library(vegan)
library(splines)
library(mgcv)
library(ggplot2)

# 加载数据
data(mite)
data(mite.env)

# 计算每个样方的物种丰富度
mite.env$Richness <- specnumber(mite)

model_quad <- lm(Richness ~ poly(SubsDens, 2), data = mite.env)
model_spline <- lm(Richness ~ ns(SubsDens, df = 3), data = mite.env)
model_gam <- gam(Richness ~ s(SubsDens), data = mite.env)
ggplot(mite.env, aes(SubsDens, Richness)) +
  geom_point(size = 2) +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), color = "blue", se = FALSE) +
  stat_smooth(method = "gam", formula = y ~ s(x), color = "red", se = FALSE) +
  stat_smooth(method = "lm", formula = y ~ ns(x, 3), color = "green", se = FALSE) +
  theme_minimal() +
  labs(title = "土壤密度对螨虫物种丰富度的非线性趋势拟合",
       x = "土壤密度 (SubsDens)", y = "物种丰富度") +
  scale_color_manual(values = c("blue", "green", "red"))

AIC(model_quad, model_spline, model_gam)

  