# 加载必要包
library(vegan)
library(nlme)
library(ggplot2)
# 数据加载与处理
data(mite)
data(mite.env)
mite.env$Shannon <- diversity(mite, index = "shannon")
model_power <- nlme(
  Shannon ~ a * SubsDens^b * WatrCont^c,
  data = mite.env,
  fixed = a + b + c ~ 1,
  random = a ~ 1 | Substrate,
  start = c(a = 5, b = 0.5, c = 0.5)
)
summary(model_power)
model_logistic <- nlme(
  Shannon ~ a / (1 + b * exp(-c * SubsDens - d * WatrCont)),
  data = mite.env,
  fixed = a + b + c + d ~ 1,
  random = a ~ 1 | Substrate,
  start = c(a = 10, b = 1,c=0.01,d=0.001)
)
summary(model_logistic)
model_logistic_complex <- nlme(
  Shannon ~ a / (1 + b * exp(-c * SubsDens - d * WatrCont)),
  data = mite.env,
  fixed = a + b + c + d ~ 1,
  random = a +d ~ 1 | Substrate,
  start = c(a = 15, b = 0.0021,c=-0.001,d=-0.005),
  
)
summary(model_logistic_complex)
AIC(model_power, model_logistic, model_logistic_complex)

# 伪R²计算函数
calc_r2 <- function(model) {
  y <- mite.env$Shannon
  y_hat <- predict(model)
  1 - sum((y - y_hat)^2) / sum((y - mean(y))^2)
}
r2_power <- calc_r2(model_power)
r2_logistic <- calc_r2(model_logistic)
r2_complex <- calc_r2(model_logistic_complex)
r2_power; r2_logistic; r2_complex

mite.env$pred <- predict(model_logistic)

ggplot(mite.env, aes(x = SubsDens, y = Shannon, color = Substrate)) +
  geom_point(size = 2) +
  geom_line(aes(y = pred), size = 1) +
  labs(
    title = "Shannon 多样性 vs 土壤密度：log-logistic 模型预测",
    x = "土壤密度 (SubsDens)",
    y = "Shannon 指数"
  ) +
  theme_minimal()
