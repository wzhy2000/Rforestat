library(nlme)
data(Loblolly)

head(Loblolly)
# 初始参数估计
start_vals <- c(Asym = 70, xmid = 10, scale = 3)

model <- nlme(
  height ~  Asym / (1 + exp(-(age - xmid) / scale)),
  data = Loblolly,
  fixed = Asym + xmid + scale ~ 1,
  random = Asym ~ 1 | Seed,
  start = start_vals
)

summary(model)
fitted_vals <- fitted(model)
observed_vals <- Loblolly$height
tss <- sum((observed_vals - mean(observed_vals))^2)
ess <- sum((fitted_vals - mean(observed_vals))^2)
r_squared <- ess / tss
r_squared


ranefs <- ranef(model)  # 提取随机效应
ranefs$Seed <- rownames(ranefs)
ranefs_sorted <- ranefs[order(-ranefs$Asym), ]

head(ranefs_sorted, 5)  # 前5个家系

# 提取固定效应 Asym
asym_fixed <- fixef(model)["Asym"]
# 提取随机效应的标准差
asym_sd <- as.numeric(VarCorr(model)[1, "StdDev"])
CV <- 100 * asym_sd / asym_fixed
CV  # 输出变异系数（%）

top_n <- ceiling(0.2 * nrow(ranefs_sorted))  # 选择前20%家系数量
mean_gain <- mean(ranefs_sorted$Asym[1:top_n])  # 平均随机效应

# 平均最大高度 = 固定效应 + 随机效应
selected_mean <- asym_fixed + mean_gain
overall_mean <- asym_fixed

delta_gain <- selected_mean - overall_mean
delta_gain  # 选择后增益

#可视化家系育种值分布
ggplot(ranefs_sorted, aes(x = reorder(Seed, -Asym), y = Asym)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "家系育种值（Asym 随机效应）", y = "育种值", x = "家系") +
  theme_minimal()

