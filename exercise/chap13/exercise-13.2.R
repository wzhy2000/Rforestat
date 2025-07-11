library(brms)
data(Loblolly)

# 模型拟合
model_lob <- brm(
  height ~ age + (1 | Seed),
  data = Loblolly,
  prior = c(
    set_prior("normal(0, 10)", class = "b"),
    set_prior("normal(0, 10)", class = "Intercept")
  ),
  chains = 4, iter = 2000, seed = 123
)

# 查看模型结果
summary(model_lob)

# 提取并比较家系随机效应
ranef_lob <- ranef(model_lob)$Seed
print(ranef_lob)

# Bayesian R² 和 WAIC
bayes_R2(model_lob)
waic(model_lob)

# 后验密度图（优良家系筛选）
plot(model_lob, pars = "^r_Seed\\[", prob = 0.95)
