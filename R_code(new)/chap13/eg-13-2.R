data(iris)
set.seed(123)

# 划分训练集和测试集
idx.train <- createDataPartition(iris$Species, p = 0.8, list = FALSE)
iris.train <- iris[idx.train, ]
iris.test <- iris[-idx.train, ]

############### 1.knn模型 ##########
library(caret)
# 设置训练控制参数，使用 10 折交叉验证
trControl <- trainControl(method = "cv", number = 10)

# 定义参数调优网格，设置 K 的取值范围
tuneGrid <- expand.grid(k = 1:10)

# 训练 KNN 模型
modeli.knn <- train(
  Species ~ .,
  data = iris.train,
  method = "knn",
  trControl = trControl,
  tuneGrid = tuneGrid
)

# 对测试集进行预测
y.pred <- predict(modeli.knn, newdata = iris.test)

# 生成混淆矩阵，评估模型性能
mat.conf <- confusionMatrix(y.pred, iris.test$Species)
print(mat.conf)

######### 2. 构建随机森林模型 ############
library(randomForest)
modeli.rf <- randomForest(Species ~ ., data=iris, importance=TRUE, proximity=TRUE)
print(modeli.rf)
y.pred <- predict(modeli.rf, newdata = iris.test)


######### 3.梯度提升树模型 #############
set.seed(123)
library(gbm)
modeli.gbm <- gbm(formula = Species ~ ., data = iris, distribution = "gaussian")
best.iter <- gbm.perf(modeli.gbm, method = "OOB")
print(modeli.gbm)
print(best.iter)

########### 4.多变量自适应回归样条 #############
set.seed(123)
library(earth)
modeli.earth <- earth(Species ~ ., data = iris)
summary(modeli.earth)
predictions <- predict(modeli.earth, type = "class")

########### 5.支持向量模型 #############
set.seed(123)
library(gbm)
modeli.svm <- svm(Species ~ ., data = iris, kernel = "linear")
summary(modeli.svm)

########### 6.神经网络模型 ###############
library(neuralnet)
modeli.nn <- neuralnet(Species ~ . , data = iris, hidden = c(5, 3), linear.output = FALSE)
plot(modeli.nn)

