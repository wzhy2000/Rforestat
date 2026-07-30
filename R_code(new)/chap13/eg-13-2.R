# 加载并处理数据
library(rattle)
library(e1071)
data(wine)
set.seed(123)
# 使用稳健的行索引写法；若需保持类别比例，应改用分层抽样。
index.train <- sample(seq_len(nrow(wine)), 0.7 * nrow(wine))
x.train <- wine[index.train, 2:ncol(wine)]
y.train <- wine[index.train, 1]
x.test <- wine[-index.train, 2:ncol(wine)]
y.test <- wine[-index.train, 1]

# 构建朴素贝叶斯模型并训练
model <- naiveBayes(x.train, as.factor(y.train))
print(model)


# 预测与评估模型
y.pred <- predict(model, x.test)
y.pred[1:10]
head(predict(model, x.test, type = "raw"))
table(y.pred, y.test)

# 继续使用wine训练数据，与后续输出保持一致。
model <- naiveBayes(x.train, as.factor(y.train), laplace = 3)
print(model)


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
