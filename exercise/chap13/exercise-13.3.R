library(vegan)
library(brms)
data(mite)
data(mite.env)

# 计算 Shannon 指数
mite.env$Shannon <- diversity(mite, index = "shannon")

# 模型 1：线性形式
model_lin <- brm(
  Shannon ~ SubsDens + WatrCont,
  data = mite.env,
  chains = 4, iter = 2000, seed = 123
)

# 模型 2：含二次项的非线性形式
model_quad <- brm(
  Shannon ~ SubsDens + WatrCont + I(SubsDens^2) + I(WatrCont^2),
  data = mite.env,
  chains = 4, iter = 2000, seed = 123
)

# LOO 模型比较
loo_lin <- loo(model_lin)
loo_quad <- loo(model_quad)
loo_compare(loo_lin, loo_quad)

# 最优模型预测区间可视化
marginal_effects(model_quad)
