library(gbm)
library(forestat)
data("crassifolia")

# AGB = Stem + Branch + Foliage + Fruit
crassifolia$AGB = crassifolia$Stem + crassifolia$Branch + crassifolia$Foliage + crassifolia$Fruit
set.seed(123)
datapartde <- sample(nrow(crassifolia), 0.7 * nrow(crassifolia))
train_data <- crassifolia[datapartde, ]
test_data <- crassifolia[-datapartde, ]
train_x <- train_data[, c("LH", "LHCB", "CPA", "D0", "H0", "HCB0", "CW")]
train_y <- train_data$AGB
test_x <- test_data[, c("LH", "LHCB", "CPA", "D0", "H0", "HCB0", "CW")]
test_y <- test_data$AGB

gbm_model <- gbm(
  formula = AGB ~ LH + LHCB + CPA + D0 + H0 + HCB0 + CW, # 回归公式
  data = train_data,                                     # 训练数据
  distribution = "gaussian",                             # 使用高斯分布
  n.trees = 1000,                                         # 决策树数量
  interaction.depth = 4,                                 # 最大树深度
  shrinkage = 0.01,                                      # 学习率
  cv.folds = 5,                                          # 5折交叉验证
  verbose = FALSE                                        # 关闭训练过程输出
)

pdf("gbm_iteration.pdf", width = 8, height = 5)
best_trees <- gbm.perf(gbm_model, method = "cv", plot.it = TRUE)
dev.off()
cat("最佳树数量为：", best_trees, "\n")

predictions <- predict(gbm_model, newdata = test_x, n.trees = best_trees)

# 模型性能评估
mse <- mean((predictions - test_y)^2)
rsq <- cor(predictions, test_y)^2

cat("测试集 MSE：", mse, "\n")
cat("测试集 R²：", rsq, "\n")

# 此外还能使用summary函数打印模型信息，查看各个变量的贡献。以及relative.influence函数。
summary(gbm_model)
relative.influence(gbm_model)