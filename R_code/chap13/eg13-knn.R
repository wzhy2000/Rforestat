library(forestat)
library(caret)
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


knnModel <- train(train_x, train_y, method = "knn", 
                  preProcess = c("center", "scale"), # 数据标准化
                  tuneLength = 10, # 选择 10 个 k 值进行调优
                  trControl = trainControl(method = "cv", number = 10) # 10折交叉验证
                  )

predictions <- predict(knnModel, newdata = test_x)
mse <- mean((predictions - test_y)^2)
rsq <- cor(predictions, test_y)^2
cat("测试集 MSE：", mse, "\n")
cat("测试集 R²：", rsq, "\n")