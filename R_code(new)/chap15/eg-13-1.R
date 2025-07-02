# remotes::install_github("FoRTExperiment/fortedata", dependencies = TRUE)
library(fortedata)
library(dplyr)
library(caret)
library(tidyverse)

##############  1. 数据加载与预处理   ###################

# 1.缺失值预处理
data <- fd_soil_respiration()
str(data)

data <- data %>%
  select(-date, -timestamp)


# 缺失值
sapply(data, function(x) sum(is.na(x)))

# data <- data %>%
#   select(plot, soil_co2_efflux, soil_temp, vwc) %>%
#   drop_na()

data <- data %>%
      drop_na()


# 2. 数值型特征预处理
# 异常值 IQR
Q1 <- quantile(data$vwc, 0.25)
Q3 <- quantile(data$vwc, 0.75)
IQR.value <- Q3 - Q1
lower.bound <- Q1 - 1.5 * IQR.value
upper.bound <- Q3 + 1.5 * IQR.value

outliers.IQR <- data$vwc[data$vwc < lower.bound | data$vwc > upper.bound]
head(outliers.IQR)
data.no.outliers <- data[data$vwc >= lower.bound & data$vwc <= upper.bound, ]

# 异常值z-score
# mean.vwc <- mean(data$vwc, na.rm = TRUE)
# sd.vwc <- sd(data$vwc, na.rm = TRUE)
# z.scores <- (data$vwc - mean.vwc) / sd.vwc
# outliers.zscore <- data$vwc[abs(z.scores) > 3]
# head(outliers.zscore)

# 偏度检验
library(e1071)
skewness(data.no.outliers$vwc)

vwc.boxcox <- BoxCoxTrans(data.no.outliers$vwc)
vwc.boxcox

vwc.boxcoxed <- predict(vwc.boxcox, as.numeric(data.no.outliers$vwc))
head(vwc.boxcoxed)
skewness(vwc.boxcoxed)


# 标准化
vwc.scaled <- scale(vwc.boxcoxed, center = T, scale = T)
attributes(vwc.scaled)


# 3.分类变量哑变量编码
data.no.outliers$plot <- as.factor(data.no.outliers$plot)
data.no.outliers$run <- as.factor(data.no.outliers$run)
dummies <- dummyVars(~ . - soil_temp - soil_co2_efflux - vwc, data = data.no.outliers, fullRank = TRUE)
transformed.data <- predict(dummies, newdata = data.no.outliers)

data <- cbind(data.frame(transformed.data),
              soil_temp = data.no.outliers$soil_temp, 
              vwc = data.no.outliers$vwc, 
              soil_co2_efflux = data.no.outliers$soil_co2_efflux)


# 4.特征降维与提取
library(psych)
selected.vars <- data[, c("soil_temp", "vwc")]
KMO(selected.vars)
psych::cortest.bartlett(selected.vars)

pca.selected.vars <- prcomp(selected.vars, scale. = T, center = T)
pca.selected.vars


# 5.特征选择
# （1）过滤法
data.num <- data[, c("soil_temp", "vwc")]
nzv <- nearZeroVar(data.num, freqCut = 20, uniqueCut = 10, saveMetrics = TRUE)
nzv

# （2）共线性检验
cor.matrix <- cor(data.num)
library(corrplot)
# corrplot(cor.matrix, order = "hclust", tl.col = "black", is.corr = T)
# pdf("土壤CO2通量数据集特征之间相关性图.pdf", width = 10, height = 8)
corrplot(cor.matrix, order = "hclust", method = "circle", type = "full", 
         addCoef.col = "red", # 设置相关系数数字的颜色
         number.cex = 1.5,      # 设置数字大小
         tl.cex = 1.5,        # 坐标轴标签的字体大小
         tl.col = "black",     # 坐标轴标签的颜色
         cl.cex = 1.5)        # 颜色图例字体大小
# dev.off()


highCorr <- findCorrelation(cor.matrix, cutoff = 0.6)
highCorr

# forestfires_num_filtered <- forestfires_num[, -highCorr]


############## 2. 样本预处理 #################
# 数据集分割
# （1）留出法（训练集与测试集）
set.seed(123)
train.idx <- createDataPartition(data$soil_co2_efflux, p = 0.8, list = FALSE)
train <- data[train.idx, ]
test <- data[-train.idx, ]
cat("训练集样本数：", nrow(train), "\n")
cat("测试集样本数：", nrow(test), "\n")
cat("总样本数：", nrow(data), "\n")

# （2）交叉验证（训练集 -> 训练集 + 验证集）
cv.ctrl <- trainControl(method = "cv", number = 5)

dummy.model <- train(
  x = train,
  y = as.factor(train$soil_co2_efflux),
  method = "rpart",
  trControl = cv.ctrl
)

for (i in seq_along(dummy.model$control$index)) {
  train.idx <- dummy.model$control$index[[i]]
  valid.idx <- dummy.model$control$indexOut[[i]]
  cat(sprintf("第 %d 折：训练集 = %d，验证集 = %d\n", 
              i, length(train.idx), length(valid.idx)))
}

cat("总样本数：", nrow(train), "\n")


# （3）自助法Bootstrap
set.seed(123)
bootstrapSamples <- createResample(train$soil_co2_efflux, times = 3)
str(bootstrapSamples)

bootTrain1 <- train[bootstrapSamples[[1]], ]
nrow(bootTrain1)

oobIndex <- setdiff(1:nrow(train), unique(bootstrapSamples[[1]]))
bootOOB1 <- train[oobIndex, ]
nrow(bootOOB1)


# 2. 异常样本剔除  
# （1） 孤立森林
library(isotree)
train.iso <- train[, -ncol(train)]
model.iso <- isolation.forest(train.iso, ntrees = 100)
scores.train.iso <- predict(model.iso, train.iso)
outliers.train.iso <- which(scores.train.iso > 0.6)
length(outliers.train.iso)

test.iso <- test[, -ncol(test)]
scores.test.iso <- predict(model.iso, test.iso)
outliers.test.iso <- which(scores.test.iso > 0.6)
length(outliers.test.iso)

# （2） 局部离群因子 LOF
library(Rlof)
num.scaled <- scale(data[, c("soil_temp", "vwc")])
x.scaled <- cbind(
  data[, !(names(data) %in% c("soil_temp", "vwc", "soil_co2_efflux"))],
  scale(data[, c("soil_temp", "vwc")])
)
scores.lof <- lof(x.scaled, k = 20)
outliers.lof <- which(scores.lof > 1.8)
length(outliers.lof)


################ 3. 模型构建   ################
# （1）线性回归模型
train$log_efflux <- log1p(train$soil_co2_efflux)
lm.reg.model <- train(log_efflux ~ soil_temp + vwc, data = train, method = "lm", trControl = cv.ctrl)
print(lm.reg.model)

# （2）岭回归模型
library(glmnet)
x.train.ridge <- as.matrix(train[, names(train) %in% c("soil_temp", "vwc")])
y.train.ridge <- as.matrix(train[, names(train) %in% c("log_efflux")])
ridge.reg.model <- cv.glmnet(x.train.ridge, y.train.ridge, alpha = 0, nfolds = 5)
par(mar = c(5,5,4,2))
# pdf("ridge.pdf", width = 10, height = 6)
plot(ridge.reg.model, cex.lab = 1.8, cex.axis = 1.8)
# dev.off()
print(ridge.reg.model)
# y_pred <- predict(ridge.reg.model, newx = x.train.ridge, s = "lambda.min")


# （3）非线性回归模型
nls.reg.model <- nls(log_efflux ~ a*(1-exp(-b*soil_temp-c*vwc)), 
                     start = c(a =1, b = 0.1, c = 0.01), data = train)
summary(nls.reg.model)
# y_pred <- predict(nls.reg.model)



# （4）随机森林回归
library(randomForest)

rf.reg.model <- train(log_efflux ~ soil_temp + vwc, data = train, method = "rf", trControl = cv.ctrl)
print(rf.reg.model)
# y_pred <- predict(rf.reg.model)


################# 4. 模型评估 ####################

evaluate.regression <- function(y.true, y.pred) {
  # 检查输入是否长度一致
  if (length(y.true) != length(y.pred)) {
    stop("y.true 和 y.pred 必须长度一致")
  }
  # 样本数
  n <- length(y.true)
  # 误差项
  residuals <- y.true - y.pred
  # 均方误差
  mse <- mean(residuals^2)
  # 均方根误差
  rmse <- sqrt(mse)
  
  # 平均绝对误差
  mae <- mean(abs(residuals))
  # 决定系数 R²
  ss_total <- sum((y.true - mean(y.true))^2)
  ss_res <- sum(residuals^2)
  r_squared <- 1 - ss_res / ss_total
  # 返回结果
  result <- data.frame(
    MSE = mse,
    RMSE = rmse,
    MAE = mae,
    R_squared = r_squared
  )
  return(result)
}


pre.lm.train <- predict(lm.reg.model, newdata = train)
evaluate.regression(train$log_efflux, pre.lm.train)

pre.ridge.train <- predict(ridge.reg.model, newx = x.train.ridge, s = "lambda.min")
evaluate.regression(train$log_efflux, pre.ridge.train)

pre.nls.train <- predict(nls.reg.model)
evaluate.regression(train$log_efflux, pre.nls.train)

pre.rf.train <- predict(rf.reg.model)
evaluate.regression(train$log_efflux, pre.rf.train)




################# 5. 参数估计  ########################
summary(lm.reg.model)
confint(lm.reg.model$finalModel, level = 0.95)


############### 6. 结果分析 ###########################
test$log_efflux <- log1p(test$soil_co2_efflux)

pre.rf.test <- predict(rf.reg.model, newdata = test)
evaluate.regression(test$log_efflux, pre.rf.test)



# 残差图
# 残差图
residuals.reg.train <- pre.rf.train - train$log_efflux
par(mar = c(5,5,4,2))
pdf("CO2训练集残差图.pdf", width = 8, height = 8, family = "GB1")
plot(pre.rf.train, residuals.reg.train,
     xlab = "土壤CO2通量",
     ylab = "残差",
     cex.lab =1.8, cex.axis = 1.8)
abline(h = 0, col = "red", lty = 2)
dev.off()


residuals.reg.test <- pre.rf.test - test$log_efflux
par(mar = c(5,5,4,2))
pdf("CO2测试集残差图.pdf", width = 8, height = 8, family = "GB1")
plot(pre.rf.test, residuals.reg.test,
     xlab = "土壤CO2通量",
     ylab = "残差",
     cex.lab =1.8, cex.axis = 1.8)
abline(h = 0, col = "red", lty = 2)
dev.off()

pdf("CO2通量-土壤体积含水量train.pdf", width = 8, height = 8, family = "GB1")
plot(train$vwc, residuals.reg.train,
     xlab = "土壤体积含水量",ylab = "残差", 
     cex.lab =1.8, cex.axis = 1.8)
dev.off()

pdf("CO2通量-土壤体积含水量test.pdf", width = 8, height = 8, family = "GB1")
plot(test$vwc, residuals.reg.test,
     xlab = "土壤体积含水量",ylab = "残差", 
     cex.lab =1.8, cex.axis = 1.8)

dev.off()



coef.summary <- summary(lm.reg.model$finalModel)$coefficients
print(coef.summary)



