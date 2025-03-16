# 加载并处理数据
library(rattle)
data(wine)
X <- wine[, 2:ncol(wine)]
y <- wine[, 1]
set.seed(6)
train_index <- sample(1:nrow(wine), 0.7 * nrow(wine))
X_train <- X[train_index, ]
y_train <- y[train_index]
X_test <- X[-train_index, ]
y_test <- y[-train_index]

# 构建朴素贝叶斯模型并训练
model <- naiveBayes(X_train, as.factor(y_train))

# 预测与评估模型
y_pred <- predict(model, X_test)
accuracy <- mean(y_pred == y_test)
cat("Model Accuracy:", round(accuracy, 2), "\n")

# 计算混淆矩阵
library(caret)
cm <- confusionMatrix(as.factor(y_pred), as.factor(y_test))
print(cm)

# 绘制混淆矩阵
library(ggplot2)
library(reshape2)
cm <- matrix(c(18, 0, 0,
               1, 19, 0,
               0, 1, 15), 
             nrow = 3, byrow = TRUE,
             dimnames = list(Prediction = c("1", "2", "3"),
                             Reference = c("1", "2", "3")))
cm_melted <- melt(cm)
ggplot(data = cm_melted, aes(x = Reference, y = Prediction, fill = value)) +
  geom_tile() +
  geom_text(aes(label = value), color = "black", size = 6) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Confusion Matrix",
       x = "Reference",
       y = "Prediction",
       fill = "Count") +
  theme_minimal()
