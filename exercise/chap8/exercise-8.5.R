# 习题 8.5：male:female 组合方差的参数自助检验

library(agridat)
library(lme4)
library(pbkrtest)
data("hanover.whitepine", package = "agridat")
d <- transform(
  hanover.whitepine,
  rep = factor(rep), male = factor(male), female = factor(female)
)

# （1）不含 male:female 的基准模型。
base_model <- lmer(
  length ~ 1 + (1 | rep) + (1 | male) + (1 | female),
  data = d, REML = TRUE
)
print(VarCorr(base_model))

# （2）只新增 male:female 随机效应。
extended_model <- update(base_model, . ~ . + (1 | male:female))
print(VarCorr(extended_model))
cat("扩展模型是否奇异：", isSingular(extended_model), "\n")

# （3）零方差位于参数边界，使用参数自助法而非普通卡方近似。
set.seed(123)
n_simulations <- 199L
bootstrap_test <- PBmodcomp(extended_model, base_model, nsim = n_simulations, seed = 123)
print(bootstrap_test)
cat("参数自助模拟次数：", n_simulations, "；p 值存在 Monte Carlo 不确定性。\n")
cat("正式出版分析建议提高到至少 1 999 次模拟。\n")

# （4）组合方差描述特定父母组合的额外偏离，不是 female:rep。
cat("male:female 方差不能直接等同特殊配合力或育种决策，需要结合交配设计与重复试验解释。\n")
