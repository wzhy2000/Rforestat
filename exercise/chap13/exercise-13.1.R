library(fortedata)
library(rstanarm)
library(bayesplot)
library(ggplot2)

# 准备数据
data <- fd_soil_respiration()
df <- na.omit(data.frame(
  log_efflux = log(data$efflux),
  soil_temp = data$soil_temp,
  vwc = data$vwc
))

# 贝叶斯线性回归（弱信息先验）
model <- stan_glm(
  log_efflux ~ soil_temp + vwc,
  data = df,
  prior = normal(0, 10),
  prior_intercept = normal(0, 10),
  chains = 4, iter = 2000, seed = 123
)

# 模型摘要
summary(model)

# 后验预测检查
pp_check(model)

# 预测值 vs 实际值可视化
df$pred <- posterior_predict(model) %>% apply(2, mean)
ggplot(df, aes(x = log_efflux, y = pred)) +
  geom_point(alpha = 0.6) +
  geom_abline(linetype = "dashed") +
  labs(title = "预测 vs 实际（log CO₂ 通量）", x = "实际值", y = "预测均值") +
  theme_minimal()
