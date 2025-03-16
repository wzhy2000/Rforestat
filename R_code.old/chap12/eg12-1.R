# 加载数据
library(e1071)
data(iris)
# 模型构建
model <- naiveBayes(Species ~ ., data = iris)
# 模型预测
predict(model, iris[1:10,])
predict(model, iris[1:10,], type = "raw")
# 模型评估
pred <- predict(model, iris)
table(pred, iris$Species)
# 模型优化
model <- naiveBayes(Species ~ ., data = iris, laplace = 3)
summary(model)

