library(tidyverse)
library(caret)
library(e1071)         # SVM
library(randomForest)  # Random Forest
library(glmnet)
library(caret)         # 模型训练与交叉验证
library(ggplot2)       # 可视化
library(dplyr)         # 数据处理
library(corrplot)
library(cowplot)

############ 1 数据预处理 ###############
data <- read.csv("forestfires.csv")
colnames(data)


# 将area进行log(x+1)变换
data$log_area <- log(data$area + 1)

data_subset <- data[, c("FFMC", "DMC", "DC", "ISI", "temp", "RH", "wind", "rain", "log_area")]
# data_subset <- data[, c("temp", "RH", "wind", "rain", "log_area")]

preProc <- preProcess(data_subset[, -9], method = c("center", "scale"))
scaled_data <- predict(preProc, data_subset)
scaled_data$log_area <- data_subset$log_area  # 加回目标变量

# 1. 使用原始数据进行划分
set.seed(123)
train_index <- createDataPartition(data_subset$log_area, p = 0.8, list = FALSE)

# 2. 划分原始数据集
train_data <- data_subset[train_index, ]
test_data <- data_subset[-train_index, ]

# 3. 使用相同的索引划分标准化数据集
train_data_scaled <- scaled_data[train_index, ]
test_data_scaled <- scaled_data[-train_index, ]

############## 2 回归模型构建 ################
########### 线性回归模型
lm_model <- lm(log_area ~ FFMC + DMC + DC + ISI + temp + RH + wind + rain,
               data = train_data)
summary(lm_model)

# x <- model.matrix(log_area ~ ., data = train_data)[, -1]  # 去掉截距列
# y <- train_data$log_area
# # 岭回归（alpha = 0）
# ridge_model <- cv.glmnet(x, y, alpha = 0)
# summary(ridge_model)
# # Lasso 回归（alpha = 1）
# lasso_model <- cv.glmnet(x, y, alpha = 1)
# summary(lasso_model)

########### 随机森林
rf_model <- randomForest(log_area ~ FFMC + DMC + DC + ISI + temp + RH + wind + rain, 
                         data = train_data, ntree = 500, importance = TRUE)

########### 神经网络

# 设置训练控制参数：10折交叉验证
train_control <- trainControl(method = "cv", number = 10)

# 设置神经网络参数范围（隐藏层神经元数 H）
tune_grid <- expand.grid(
  size = c(2, 4, 6, 8, 10),  # 隐藏层神经元数
  decay = 0.01                # 不使用正则化惩罚（可根据需要修改）
)

# caret::train函数将自动使用BFGS算法训练nnet模型
# 重复训练3次，每次保留预测结果，最后求平均
results_list <- list()
for (i in 1:3) {
  set.seed(123 + i)  # 每次略微改变随机种子以避免同一个局部最优
  
  model <- train(
    log_area ~ FFMC + DMC + DC + ISI + temp + RH + wind + rain,  # 
    data = train_data_scaled,
    method = "nnet",
    trControl = train_control,
    tuneGrid = tune_grid,
    linout = TRUE,     # 表示是回归任务
    trace = FALSE,
    maxit = 500        # 相当于100个epoch
  )
  results_list[[i]] <- model
}


# 查看每次最优的 H 值
sapply(results_list, function(x) x$bestTune$size)


########### 支持向量回归

svm_control <- trainControl(method = "cv", number = 10)
tune_grid <- expand.grid(
  C = 3,
  sigma = 2^c(-9, -7, -5, -3, -1)  # kernlab中 gamma 被叫做 sigma
)
svm_model <- train(
  log_area ~ .,
  data = train_data_scaled,
  method = "svmRadial",
  preProcess = NULL,   # 数据已预处理
  trControl = train_control,
  tuneGrid = tune_grid
)


########## 3 模型选择    #############


# 在 evaluate 函数中增加反变换
evaluate <- function(pred_log, true_log) {
  pred <- pred_log
  true <- true_log
  
  mad <- mean(abs(pred - true))
  rmse <- sqrt(mean((pred - true)^2))
  r2 <- 1 - sum((pred - true)^2) / sum((true - mean(true))^2)
  
  # 返回一个字符串，包含评估结果
  return(paste("MAD:", round(mad, 3), "RMSE:", round(rmse, 3)))
}


# 测试不同模型的表现
pred_test_lm <- predict(lm_model, test_data)  # 线性回归模型预测
pred_test_rf <- predict(rf_model, test_data)  # 随机森林模型预测
pred_test_nn <- apply(sapply(results_list, function(model) { predict(model, newdata = test_data_scaled) }), 1, mean)
# 神经网络模型预测
pred_test_svm <- predict(svm_model, test_data_scaled)  # 支持向量回归

y_test <- test_data$log_area


cat("Linear Regression:", evaluate(pred_test_lm, y_test), "\n")
cat("Random Forest:", evaluate(pred_test_rf, y_test), "\n")
cat("Neural Network:", evaluate(pred_test_nn, y_test), "\n")
cat("SVM:", evaluate(pred_test_svm, y_test), "\n")

# 训练集预测
# 线性回归模型预测（训练集）
pred_train_lm <- predict(lm_model, train_data)

# 随机森林模型预测（训练集）
pred_train_rf <- predict(rf_model, train_data)

# 神经网络模型预测（训练集）
pred_train_nn <- apply(sapply(results_list, function(model) { predict(model, newdata = train_data_scaled) }), 1, mean)

# 支持向量机模型预测（训练集）
pred_train_svm <- predict(svm_model, train_data_scaled)


# 训练集绘制残差图
y_train <- train_data$log_area
p1 <- ggplot(mapping = aes(x = pred_train_lm, y = y_train - lm_pred_train)) +
  geom_point(alpha = 0.6, size = 1.5, color = "black") +  # 设置点的颜色
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 1) +
  labs(title = "线性回归模型训练集",
       x = "预测值 (log_area)",
       y = "残差") +
  theme_classic() +
  theme(text = element_text(size = 16))


p2 <- ggplot(mapping = aes(x = pred_train_rf, y = y_train - pred_train_rf)) +
  geom_point(alpha = 0.6, size = 1.5, color = "black") +  # 设置点的颜色
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 1) +
  labs(title = "随机森林模型训练集",
       x = "预测值 (log_area)",
       y = "残差") +
  theme_classic() +
  theme(text = element_text(size = 16))


p3 <- ggplot(mapping = aes(x = pred_train_nn, y = y_train - pred_train_nn)) +
  geom_point(alpha = 0.6, size = 1.5, color = "black") +  # 设置点的颜色
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 1) +
  labs(title = "神经网络模型训练集",
       x = "预测值 (log_area)",
       y = "残差") +
  theme_classic() +
  theme(text = element_text(size = 16))


p4 <- ggplot(mapping = aes(x = pred_train_svm, y = y_train - pred_train_svm)) +
  geom_point(alpha = 0.6, size = 1.5, color = "black") +  # 设置点的颜色
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 1) +
  labs(title = "支持向量机模型训练集",
       x = "预测值 (log_area)",
       y = "残差") +
  theme_classic() +
  theme(text = element_text(size = 16))


# 测试集残差图
p5 <- ggplot(mapping = aes(x = pred_test_lm, y = y_test - pred_test_lm)) +
  geom_point(alpha = 0.6, size = 1.5, color = "black") +  # 设置点的颜色
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 1) +
  labs(title = "线性回归模型测试集",
       x = "预测值 (log_area)",
       y = "残差") +
  theme_classic() 
  theme(text = element_text(size = 16))

p6 <- ggplot(mapping = aes(x = pred_test_rf, y = y_test - pred_test_rf)) +
  geom_point(alpha = 0.6, size = 1.5, color = "black") +  # 设置点的颜色
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 1) +
  labs(title = "随机森林模型测试集",
       x = "预测值 (log_area)",
       y = "残差") +
  theme_classic() +
  theme(text = element_text(size = 16))

p7 <- ggplot(mapping = aes(x = pred_test_nn, y = y_test - pred_test_nn)) +
  geom_point(alpha = 0.6, size = 1.5, color = "black") +  # 设置点的颜色
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 1) +
  labs(title = "神经网络模型测试集",
       x = "预测值 (log_area)",
       y = "残差") +
  theme_classic() +
  theme(text = element_text(size = 16))

p8 <- ggplot(mapping = aes(x = pred_test_svm, y = y_test - pred_test_svm)) +
  geom_point(alpha = 0.6, size = 1.5, color = "black") +  # 设置点的颜色
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 1) +
  labs(title = "支持向量机模型测试集",
       x = "预测值 (log_area)",
       y = "残差") +
  theme_classic() +
  theme(text = element_text(size = 16))


plots <- plot_grid(p1, p2, p3, p4, p5, p6, p7, p8, ncol = 4)
plots

pdf("D:\\大连理工大学\\R语言书稿\\Rforestat\\R_code(new)\\扩展\\四种模型残差图.pdf", width = 14, height = 7, family="GB1")
plots
dev.off()
