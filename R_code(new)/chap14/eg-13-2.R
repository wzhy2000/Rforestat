library(caret)

data <- get(load("JFSP_all_df.rda"))
set.seed(123)

# 1. 先划分训练集和测试集（保留 LiveDead 标签的分布）
idx <- createDataPartition(data$LiveDead, p = 0.7, list = FALSE)
train.raw <- data[idx, ]
test.raw  <- data[-idx, ]

# 2. 对树种变量进行 one-hot 编码（只在训练集上构建 dummyVars 模型）
dmy <- dummyVars(~ Species, data = train.raw)
train.sp <- predict(dmy, train.raw)
test.sp  <- predict(dmy, test.raw)

# 3. 提取连续变量
num_vars <- c("heat_load", "slope", "roughness", "wyr1_suVPDmu")
train.num <- train.raw[, num_vars]
test.num  <- test.raw[, num_vars]

# 4. 在训练集上拟合 Z-score 标准化参数，并应用到测试集
preProcValues <- preProcess(train.num, method = c("center", "scale"))
train.num.scaled <- predict(preProcValues, train.num)
test.num.scaled  <- predict(preProcValues, test.num)

# 5. 合并连续变量 + one-hot 编码 + 响应变量
train <- as.data.frame(cbind(train.sp, train.num.scaled))
train$LiveDead <- factor(train.raw$LiveDead, levels = c(0,1), labels = c("Alive", "Dead"))

test <- as.data.frame(cbind(test.sp, test.num.scaled))
test$LiveDead <- factor(test.raw$LiveDead, levels = c(0,1), labels = c("Alive", "Dead"))



############### 1.knn模型 ##########
library(caret)
# 设置训练控制参数，使用 10 折交叉验证
trControl <- trainControl(method = "cv", number = 10)

# 定义参数调优网格，设置 K 的取值范围
tuneGrid <- expand.grid(k = 1:10)

# 训练 KNN 模型
modeli.knn <- train(
  LiveDead ~ ., data = train,
  method = "knn",
  trControl = trControl,
  tuneGrid = tuneGrid
)

# 对测试集进行预测
y.pred <- predict(modeli.knn, newdata = test)

# 生成混淆矩阵，评估模型性能
mat.conf <- confusionMatrix(y.pred, test$LiveDead, positive = "Dead")
print(mat.conf)

######### 2. 构建随机森林模型 ############
library(randomForest)
modeli.rf <- randomForest(LiveDead ~ ., data = train, importance=TRUE, proximity=TRUE)
print(modeli.rf)
y.pred <- predict(modeli.rf, newdata = test)


######### 3.梯度提升树模型 #############
set.seed(123)
library(gbm)
train$LiveDead <- ifelse(train$LiveDead == "Dead", 1, 0)
test$LiveDead  <- ifelse(test$LiveDead == "Dead", 1, 0)
modeli.gbm <- gbm(LiveDead ~ ., data = train, distribution = "bernoulli")
best.iter <- gbm.perf(modeli.gbm, method = "OOB")
print(modeli.gbm)
print(best.iter)

########### 4.多变量自适应回归样条 #############
set.seed(123)
library(earth)
train$LiveDead <- factor(train.raw$LiveDead, levels = c(0,1), labels = c("Alive", "Dead"))
test$LiveDead <- factor(test.raw$LiveDead, levels = c(0,1), labels = c("Alive", "Dead"))
modeli.earth <- earth(LiveDead ~ ., data = train)
summary(modeli.earth)
predictions <- predict(modeli.earth, newdata = test, type = "class")

########### 5.支持向量模型 #############
set.seed(123)
library(e1071)
modeli.svm <- svm(LiveDead ~ ., data = train, kernel = "linear")
summary(modeli.svm)

########### 6.神经网络模型 ###############
library(neuralnet)
set.seed(123)
modeli.nn <- neuralnet(LiveDead ~ ., data = train, hidden = c(5, 3), 
                       linear.output = FALSE, stepmax = 100000)

plot(modeli.nn, col.entry = "blue", col.hidden = "green", col.out = "red", arrow.length = 0.2, cex = 0.8, information = FALSE, show.weights = FALSE)


















# data(iris)
# set.seed(123)
# 
# # 划分训练集和测试集
# idx.train <- createDataPartition(iris$Species, p = 0.8, list = FALSE)
# iris.train <- iris[idx.train, ]
# iris.test <- iris[-idx.train, ]
# 
# ############### 1.knn模型 ##########
# library(caret)
# # 设置训练控制参数，使用 10 折交叉验证
# trControl <- trainControl(method = "cv", number = 10)
# 
# # 定义参数调优网格，设置 K 的取值范围
# tuneGrid <- expand.grid(k = 1:10)
# 
# # 训练 KNN 模型
# modeli.knn <- train(
#   Species ~ .,
#   data = iris.train,
#   method = "knn",
#   trControl = trControl,
#   tuneGrid = tuneGrid
# )
# 
# # 对测试集进行预测
# y.pred <- predict(modeli.knn, newdata = iris.test)
# 
# # 生成混淆矩阵，评估模型性能
# mat.conf <- confusionMatrix(y.pred, iris.test$Species)
# print(mat.conf)
# 
# ######### 2. 构建随机森林模型 ############
# library(randomForest)
# modeli.rf <- randomForest(Species ~ ., data=iris, importance=TRUE, proximity=TRUE)
# print(modeli.rf)
# y.pred <- predict(modeli.rf, newdata = iris.test)
# 
# 
# ######### 3.梯度提升树模型 #############
# set.seed(123)
# library(gbm)
# modeli.gbm <- gbm(formula = Species ~ ., data = iris, distribution = "gaussian")
# best.iter <- gbm.perf(modeli.gbm, method = "OOB")
# print(modeli.gbm)
# print(best.iter)
# 
# ########### 4.多变量自适应回归样条 #############
# set.seed(123)
# library(earth)
# modeli.earth <- earth(Species ~ ., data = iris)
# summary(modeli.earth)
# predictions <- predict(modeli.earth, type = "class")
# 
# ########### 5.支持向量模型 #############
# set.seed(123)
# library(gbm)
# modeli.svm <- svm(Species ~ ., data = iris, kernel = "linear")
# summary(modeli.svm)
# 
# ########### 6.神经网络模型 ###############
# library(neuralnet)
# modeli.nn <- neuralnet(Species ~ . , data = iris, hidden = c(5, 3), linear.output = FALSE)
# plot(modeli.nn)
# 
