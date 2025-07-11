# 加载必要的包
library(vegan)
library(nlme)
library(ggplot2)
library(dplyr)

# 加载数据
data(dune)
data(dune.env)

# 计算总盖度（每行代表一个样地）
dune.env$Abundance <- rowSums(dune)

# 合并数据：计算每个样地的总盖度
df <- data.frame(
  site = factor(1:nrow(dune)),
  abundance = rowSums(dune),               # 总盖度（响应变量）
  moisture = as.numeric(as.character(dune.env$Moisture)),  # 湿度（解释变量）
  management = dune.env$Management         # 管理方式（随机效应分组）
)


# 非线性混合效应模型
model <- nlme(abundance ~ a * moisture^b,
              data = df,
              fixed = a + b ~ 1,
              random = a ~ 1 | management,
              start = c(a = 36.2819, b = -0.061))  # 使用 nls 拟合结果作为起点


summary(model)


fitted_vals <- fitted(model)
observed_vals <- df$abundance

rss <- sum((observed_vals - fitted_vals)^2)
tss <- sum((observed_vals - mean(observed_vals))^2)

r_squared <- 1 - rss / tss
r_squared

# 随机效应标准差（管理方式对截距a的扰动）
stddev_a <- as.numeric(VarCorr(model)[1, "StdDev"])
fix_a <- fixef(model)["a"]

cv_percent <- 100 * stddev_a / fix_a
cv_percent

ranefs <- ranef(model)
ranefs$Management <- rownames(ranefs)

ggplot(ranefs, aes(x = reorder(Management, a), y = a)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  labs(title = "各管理方式的基础盖度偏移", x = "Management", y = "随机效应 a") +
  coord_flip() +
  theme_minimal()

resids <- residuals(model, type = "normalized")
ggplot(data.frame(fit = fitted_vals, res = resids),
       aes(x = fit, y = res)) +
  geom_point() +
  geom_hline(yintercept = 0, col = "red") +
  labs(title = "残差图：检验异方差性", x = "Fitted", y = "Residuals") +
  theme_minimal()

model_varPower <- update(model,
                         weights = varPower()
)

summary(model_varPower)
