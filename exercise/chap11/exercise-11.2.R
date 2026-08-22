# 习题 11.2：Loblolly Logistic 生长曲线的 Seed 随机差异

library(nlme)
data("Loblolly", package = "datasets")
d <- Loblolly

stopifnot(nlevels(d$Seed) == 14L, all(table(d$Seed) == 6L))
cat("树木（Seed）数：", nlevels(d$Seed), "；每棵树观测次数：", unique(as.integer(table(d$Seed))), "\n")

model <- nlme(
  height ~ Asym / (1 + exp((xmid - age) / scal)),
  data = d,
  fixed = Asym + xmid + scal ~ 1,
  random = Asym ~ 1 | Seed,
  start = c(Asym = 62, xmid = 12, scal = 4),
  method = "ML",
  control = nlmeControl(maxIter = 200, pnlsMaxIter = 50, msMaxIter = 200)
)
print(summary(model))
print(VarCorr(model))

# （1）Asym 随机效应条件预测值：这是相对总体 Asym 的收缩偏离。
asym_random <- ranef(model)
random_effect_table <- data.frame(
  Seed = rownames(asym_random),
  Asym_random_effect = asym_random[, "Asym"],
  row.names = NULL
)
random_effect_table <- random_effect_table[order(-random_effect_table$Asym_random_effect), ]
cat("Asym 随机效应条件预测值最高的前5个 Seed：\n")
print(head(random_effect_table, 5), row.names = FALSE)

# （2）组间变异系数 CV% = SD(Asym 随机效应) / 总体固定 Asym * 100。
asym_random_sd <- as.numeric(VarCorr(model)["Asym", "StdDev"])
fixed_asym <- unname(fixef(model)["Asym"])
asym_cv_percent <- 100 * asym_random_sd / fixed_asym
cat("总体固定 Asym：", fixed_asym, "\n")
cat("Seed 间 Asym 随机效应标准差：", asym_random_sd, "\n")
cat("Seed 分组间变异系数 CV%：", asym_cv_percent, "\n")

# （3）按条件 Asym（固定效应 + 随机效应）选择最高的前20%。
conditional_parameters <- coef(model, level = 1)
conditional_asym_table <- data.frame(
  Seed = rownames(conditional_parameters),
  Asym = conditional_parameters[, "Asym"],
  row.names = NULL
)
conditional_asym_table <- conditional_asym_table[order(-conditional_asym_table$Asym), ]
top_n <- max(1L, ceiling(0.20 * nrow(conditional_asym_table)))
top_20_percent <- head(conditional_asym_table, top_n)
top_asym_mean <- mean(top_20_percent$Asym)
top_asym_difference <- top_asym_mean - fixed_asym
cat("前20%对应", top_n, "个 Seed：\n")
print(top_20_percent, row.names = FALSE)
cat("前20%条件 Asym 均值：", top_asym_mean, "\n")
cat("与总体固定 Asym 的差值：", top_asym_difference, "\n")
cat("以上排序和差值仅描述样本内树木的条件预测，不代表育种值、遗传增益或家系选择。\n")
