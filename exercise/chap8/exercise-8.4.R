# 习题 8.4：白松交配试验的随机方差分解

library(agridat)
library(lme4)
data("hanover.whitepine", package = "agridat")
d <- transform(
  hanover.whitepine,
  rep = factor(rep), male = factor(male), female = factor(female)
)

# （1）核对 4 rep x 4 male x 7 female 的平衡小区均值设计。
design_table <- with(d, table(rep, male, female))
print(dim(design_table))
print(table(as.vector(design_table)))
stopifnot(nrow(d) == 112L, all(design_table == 1L))
cat("length 的分析单位是小区上胚轴长度均值，不是单株原始观测。\n")

# （2）同时考虑 rep、male、female 和 male:female 随机变异。
model <- lmer(
  length ~ 1 + (1 | rep) + (1 | male) + (1 | female) + (1 | male:female),
  data = d, REML = TRUE
)
print(summary(model))

# （3）报告方差分量、profile 区间和奇异性。
print(VarCorr(model))
profile_interval <- tryCatch(
  confint(model, method = "profile", oldNames = FALSE),
  error = function(e) {
    message("profile 区间计算失败：", conditionMessage(e))
    confint(model, method = "Wald", oldNames = FALSE)
  }
)
print(profile_interval)
cat("模型是否奇异：", isSingular(model), "\n")

# （4）方差分量是交配设计下的变异来源，不能直接命名为个体遗传力。
cat("遗传力换算还需要交配设计系数、亲缘假设和个体尺度残差，不能直接使用方差占比。\n")
