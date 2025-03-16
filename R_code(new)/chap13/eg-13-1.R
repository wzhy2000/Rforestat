library(forestat)     # 雷达反演数据集
library(caret)        # KNN
library(randomForest) # 随机森林
library(gbm)          # 梯度提升树
library(earth)        # 多变量自适应回归样条
library(e1071)        # 支持向量机
library(neuralnet)    # 神经网络
data("picea")   # 加载雷达反演数据集

# 数据处理
# AGB = Stem + Branch + Foliage + Fruit
picea$AGB <- picea$STEM + picea$BRANCH + picea$FOLIAGE + picea$FRUIT


set.seed(123)
idx.train <- sample(nrow(picea), 0.7 * nrow(picea))
picea.train <- picea[idx.train, ]
picea.test <- picea[-idx.train, ]

x.train <- picea.train[, c("LH", "LHCB", "CPA", "D0", "H0", "HCB0", "CW")]
y.train <- picea.train$AGB
x.test <- picea.test[, c("LH", "LHCB", "CPA", "D0", "H0", "HCB0", "CW")]
y.test <- picea.test$AGB


################ 1.KNN模型预测地面生物量（AGB）#####################
modela.knn <- train(x.train, y.train, method = "knn", 
                    preProcess = c("center", "scale"), # 数据标准化
                    tuneLength = 10,                   # 选择 10 个 k 值进行调优
                    trControl = trainControl(method = "cv", number = 10) # 10折交叉验证
)

# 模型性能评估
y.pred <- predict(modela.knn, newdata = x.test)
y.mse <- mean((y.pred - y.test)^2)
y.rsq <- cor(y.pred, y.test)^2
cat("测试集 MSE：", y.mse, " R²：", y.rsq, "\n")
FittingEvaluationIndex(y.pred, picea.test$AGB)

################# 2.构建随机森林模型预测地面生物量 #################
set.seed(123)
modela.rf <- randomForest(x = x.train, y = y.train, ntree = 500, 
                          replace = TRUE, importance = TRUE)

y.pred <- predict(modela.rf, newdata = x.test)
y.mse <- mean((y.pred - y.test)^2)
y.rsq <- cor(y.pred, y.test)^2
cat("测试集 MSE：", y.mse, " R²：", y.rsq, "\n")

# 绘制误差变化图
pdf("eg_randomforest.pdf",width = 8, height = 5)
plot(modela.rf)
dev.off()

# 提取变量重要性
importance(modela.rf)

# 可视化变量重要性
varImpPlot(modela.rf)


############### 3.梯度提升树 ###################
set.seed(123)
# 使用5折交叉验证
modela.gbm <- gbm(formula = AGB ~ LH + LHCB + CPA + D0 + H0 + HCB0 + CW,
                  data = picea.train, distribution = "gaussian", 
                  n.trees = 1000, interaction.depth = 4, shrinkage = 0.01, 
                  cv.folds = 5, verbose = FALSE)


n.best <- gbm.perf(modela.gbm, method = "cv", plot.it = TRUE)
cat("最佳子树数量为：", n.best, "\n")


# 也可使用袋外估计
modela.gbm.oob <- gbm(formula = AGB ~ LH + LHCB + CPA + D0 + H0 + HCB0 + CW,
                      data = picea.train, distribution = "gaussian", n.trees = 1000, 
                      interaction.depth = 4, shrinkage = 0.01, verbose = FALSE)


# 使用袋外估计可能效果不如交叉验证，通常低估了迭代的最佳次数
n.best <- gbm.perf(modela.gbm.oob, method = "OOB", plot.it = TRUE, oobag.curve = TRUE)
cat("最佳树数量为：", n.best, "\n")

# 预测和评估
y.pred <- predict(modela.gbm, newdata = x.test)
y.mse <- mean((y.pred - y.test)^2)
y.rsq <- cor(y.pred, y.test)^2
cat("测试集 MSE：", y.mse, " R²：", y.rsq, "\n")


################# 4.多变量自适应回归样条 #####################
set.seed(123)
modela.earth <- earth(AGB ~ LH + LHCB + CPA + D0 + H0 + HCB0 + CW, data = picea.train, pmethod = "backward")

y.pred <- predict(modela.earth, newdata = x.test)
y.mse <- mean((y.pred - y.test)^2)
y.rsq <- cor(y.pred, y.test)^2
cat("测试集 MSE：", y.mse, " R²：", y.rsq, "\n")



################# 5.支持向量模型 #####################
set.seed(123)
modela.svm <- svm(AGB ~ LH + LHCB + CPA + D0 + H0 + HCB0 + CW, 
                  data = picea.train, type = "eps-regression", 
                  kernel = "radial", scale = TRUE)

# 预测和评估
y.pred <- predict(modela.svm, newdata = x.test)
y.mse <- mean((y.pred - y.test)^2)
y.rsq  <- cor(y.pred, y.test)^2
cat("测试集 MSE：", y.mse, " R²：", y.rsq, "\n")

# tune.svm模型
modela.svm.tune <- tune.svm(AGB ~ LH + LHCB + CPA + D0 + H0 + HCB0 + CW, 
                            data = picea.train, type = "eps-regression", 
                            kernel = "radial", cost = 10^(-1:2), gamma = 10^(-2:1), 
                            epsilon = c(0.1, 0.2, 0.3))

# 提取调优后的最佳模型
modela.svm.best <- modela.svm.tune$best.model
summary(modela.svm.best)

# 预测和评估
y.pred <- predict(modela.svm.best, newdata = x.test)
y.mse <- mean((y.pred - y.test)^2)
y.rsq <- cor(y.pred, y.test)^2
cat("测试集 MSE：", y.mse, " R²：", y.rsq, "\n")


############## 6.神经网络模型 ##################
#数据处理

set.seed(123)
# 目标值归一化
picea.test$AGB <- (picea.test$AGB - min(picea.train$AGB)) / (max(picea.train$AGB) - min(picea.train$AGB))
picea.train$AGB <- (picea.train$AGB - min(picea.train$AGB)) / (max(picea.train$AGB) - min(picea.train$AGB))

picea.train <- picea.train[,c("LH", "LHCB", "CPA", "D0", "H0", "HCB0", "CW", "AGB")]
picea.test <- picea.test[,c("LH", "LHCB", "CPA", "D0", "H0", "HCB0", "CW", "AGB")]


# 计算训练集的最大值和最小值
min.train <- apply(picea.train[, -ncol(picea.train)], 2, min)  # 最小值
max.train <- apply(picea.train[, -ncol(picea.train)], 2, max)  # 最大值

# 归一化函数
normalize <- function(x, min_val, max_val) {
  return((x - min_val) / (max_val - min_val))
}

# 对训练集进行归一化（可选）
for (col in names(min.train)) {
  picea.train[[col]] <- normalize(picea.train[[col]], min.train[col], max.train[col])
}

# 对测试集进行归一化
picea.test <- picea.test
for (col in names(min.train)) {
  picea.test[[col]] <- normalize(picea.test[[col]], min.train[col], max.train[col])
}



# 构建神经网络模型
modela.nn <- neuralnet(AGB ~ LH + LHCB + CPA + D0 + H0 + HCB0 + CW,
                       data = picea.train,
                       hidden = c(10, 10),
                       linear.output = TRUE,
                       err.fct = "sse",
                       act.fct = "logistic",
                       threshold = 0.0001,
                       learningrate = 0.01,
                       stepmax = 500000)



# 预测和评估
y.pred <- predict(modela.nn, newdata = picea.test)
y.mse <- mean((y.pred - picea.test$AGB)^2)
y.rsq <- cor(y.pred, picea.test$AGB)^2
cat("测试集 MSE：", y.mse, "R²：", y.rsq, "\n")



plot(modela.nn, col.entry = "blue", col.hidden = "green", col.out = "red", arrow.length = 0.2, cex = 0.8, information = FALSE, show.weights = FALSE)  



