library(BGLR)
library(ggplot2)
library(tidyr)

# 数据准备
data <- read.csv("data.csv", fileEncoding = "gbk")
y <- data$BA
X_base <- as.matrix(data[, c("D0", "VH", "CD")])
X_inter <- cbind(X_base, D0_VH = data$D0 * data$VH)  # 添加交互项

# 模拟缺失用于预测精度评估
set.seed(123)
na.idx <- sample(1:length(y), 100)
y_miss <- y
y_miss[na.idx] <- NA
# 模型1：非信息先验建模（贝叶斯岭回归 BRR）
model1 <- BGLR(
  y = y_miss,
  ETA = list(list(X = X_base, model = "BRR")),
  nIter = 1000, burnIn = 200, verbose = FALSE
)

# 模型2：偏置信念建模（BayesA + 交互项）
model2 <- BGLR(
  y = y_miss,
  ETA = list(list(X = X_inter, model = "BayesA")),
  nIter = 1000, burnIn = 200, verbose = FALSE
)

# 模型预测与评估
yhat1 <- model1$yHat[na.idx]
yhat2 <- model2$yHat[na.idx]
ytrue <- y[na.idx]

cat("【模型评估】\n")
cat("BRR模型 → R² =", round(cor(yhat1, ytrue)^2, 3),
    ", RMSE =", round(sqrt(mean((yhat1 - ytrue)^2)), 3), "\n")
cat("BayesA模型（含交互）→ R² =", round(cor(yhat2, ytrue)^2, 3),
    ", RMSE =", round(sqrt(mean((yhat2 - ytrue)^2)), 3), "\n")

# 后验分布绘图：提取系数样本
b_samples <- model2$ETA[[1]]$b

# 判断是一维 or 多维
if (is.null(dim(b_samples))) {
  # 单变量情况
  df_post <- data.frame(变量 = "D0", 样本值 = b_samples)
  ggplot(df_post, aes(x = 样本值)) +
    geom_density(fill = "steelblue", alpha = 0.6) +
    theme_minimal() +
    labs(title = "参数后验密度图（单变量）", x = "回归系数", y = "密度")
} else {
  # 多变量情况
  colnames(b_samples) <- c("D0", "VH", "CD", "D0_VH")  # 可选：给变量命名
  df_long <- pivot_longer(as.data.frame(b_samples),
                          cols = everything(),
                          names_to = "变量",
                          values_to = "样本值")
  ggplot(df_long, aes(x = 样本值, fill = 变量)) +
    geom_density(alpha = 0.6) +
    facet_wrap(~变量, scales = "free") +
    theme_minimal() +
    labs(title = "参数估计的后验密度图", x = "回归系数", y = "密度")
}