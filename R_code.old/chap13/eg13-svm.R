library(e1071)
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

svm_model <- svm(AGB ~ LH + LHCB + CPA + D0 + H0 + HCB0 + CW, 
                 data = train_data, type = "eps-regression", # 使用回归模型 (eps-regression)
                 kernel = "radial", #核函数选择 "radial" 
                 scale = TRUE   # 启用标准化数据
                 )

predictions <- predict(svm_model, newdata = test_x)
mse <- mean((predictions - test_y)^2)
rsq <- cor(predictions, test_y)^2
cat("测试集 MSE：", mse, "\n")
cat("测试集 R²：", rsq, "\n")


svm_tune <- tune.svm(
  AGB ~ LH + LHCB + CPA + D0 + H0 + HCB0 + CW,  # 目标公式
  data = train_data,                            # 训练数据
  type = "eps-regression",                      # 使用回归类型
  kernel = "radial",                            # 使用径向基核函数
  cost = 10^(-1:2),                             # C 参数的范围
  gamma = 10^(-2:1),                            # gamma 参数的范围
  epsilon = c(0.1, 0.2, 0.3)                    # epsilon 参数的范围
)

best_svm_model <- svm_tune$best.model
summary(best_svm_model)

predictions <- predict(best_svm_model, newdata = test_x)
mse <- mean((predictions - test_y)^2)
rsq <- cor(predictions, test_y)^2
cat("测试集 MSE：", mse, "\n")
cat("测试集 R²：", rsq, "\n")
