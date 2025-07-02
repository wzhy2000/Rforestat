library(dplyr)
library(caret)
library(tidyverse)
data <- read.csv("ASTER.csv")
str(data)

################ 1. 特征预处理  #################

# 1. 缺失值处理
sapply(data, function(x) sum(is.na(x)))

data <- data %>%
  drop_na()

# 2. 数值型特征预处理

# 1.异常值 
# IQR
# Q1 <- quantile(data$b1, 0.25)
# Q3 <- quantile(data$b1, 0.75)
# IQR.value <- Q3 - Q1
# lower.bound <- Q1 - 1.5 * IQR.value
# upper.bound <- Q3 + 1.5 * IQR.value
# outliers.IQR <- data$b1[data$b1 < lower.bound | data$b1 > upper.bound]
# head(outliers.IQR)
# data.no.outliers <- data[data$b1 >= lower.bound & data$b1 <= upper.bound, ]

# (1) 异常值  z-score
mean.b1 <- mean(data$b1, na.rm = TRUE)
sd.b1 <- sd(data$b1, na.rm = TRUE)
z.scores <- (data$b1 - mean.b1) / sd.b1
outliers.zscore <- data$b1[abs(z.scores) > 3]
head(outliers.zscore)
data.no.outliers <- data[abs(z.scores) <= 3, ]


# (2) 偏度检验
library(e1071)
skewness(data.no.outliers$b1)

b1.boxcox <- BoxCoxTrans(data.no.outliers$b1)
b1.boxcox

b1.boxcoxed <- predict(b1.boxcox, as.numeric(data.no.outliers$b1))
head(b1.boxcoxed)
skewness(b1.boxcoxed)


# (3) 中心化与标准化
b1.scaled <- scale(b1.boxcoxed, center = T, scale = T)
attributes(b1.scaled)


# 3. 分类变量编码
data.no.outliers$class <- factor(data.no.outliers$class)


# 4. 特征降维与提取
library(psych)
selected.vars <- data[, -c(1)]
KMO(selected.vars)
psych::cortest.bartlett(selected.vars)

pca.selected.vars <- prcomp(selected.vars, scale. = T, center = T)
pca.selected.vars$x[1:5, 1:5]


# 5. 特征选择

# (1) 过滤法
library(caret)
data.num <- data[, -c(1)]
nzv <- nearZeroVar(data.num, freqCut = 20, uniqueCut = 10, saveMetrics = TRUE)
nzv


# (2) 共线性检验
cor.matrix <- cor(data.num)
library(corrplot)
# corrplot(cor.matrix, order = "hclust", tl.col = "black", is.corr = T)
# pdf("ASTER 影像数据集特征之间相关性图.pdf", width = 10, height = 8)
corrplot(cor.matrix, order = "hclust", method = "circle", type = "full", 
         # addCoef.col = "red", # 设置相关系数数字的颜色
         number.cex = 1,      # 设置数字大小
         tl.cex = 1,        # 坐标轴标签的字体大小
         tl.col = "black",     # 坐标轴标签的颜色
         cl.cex = 1)        # 颜色图例字体大小
# dev.off()

highCorr <- findCorrelation(cor.matrix, cutoff = 0.6)
highCorr
data <- data[, -(highCorr + 1)]
colnames(data)


############### 2. 样本预处理    #####################
# 1. 样本划分
# （1）留出法（训练集与测试集）
set.seed(123)
train.idx <- createDataPartition(data$class, p = 0.8, list = FALSE)
train <- data[train.idx, ]
test <- data[-train.idx, ]
cat("训练集样本数：", nrow(train), "\n")
cat("测试集样本数：", nrow(test), "\n")
cat("总样本数：", nrow(data), "\n")

# （2）交叉验证（训练集 -> 训练集 + 验证集）
cv.ctrl <- trainControl(method = "cv", number = 5)
dummy.model <- train(
  x = train,
  y = as.factor(train$class),
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
bootstrapSamples <- createResample(train$class, times = 3)
str(bootstrapSamples)

bootTrain1 <- train[bootstrapSamples[[1]], ]
nrow(bootTrain1)

oobIndex <- setdiff(1:nrow(train), unique(bootstrapSamples[[1]]))
bootOOB1 <- train[oobIndex, ]
nrow(bootOOB1)



# 2. 异常样本剔除
# （1） 孤立森林
library(isotree)
train.iso <- train[, -c(1)]
model.iso <- isolation.forest(train.iso, ntrees = 100)
scores.train.iso <- predict(model.iso, train.iso)
outliers.train.iso <- which(scores.train.iso > 0.6)
length(outliers.train.iso)
train.iso <- train.iso[-outliers.train.iso, ]

test.iso <- test[, -c(1)]
scores.test.iso <- predict(model.iso, test.iso)
outliers.test.iso <- which(scores.test.iso > 0.6)
length(outliers.test.iso)

# （2） 局部离群因子 LOF
library(Rlof)
x.scaled <- data %>% select(-class) %>% scale()
scores.lof <- lof(x.scaled, k = 20)
outliers.lof <- which(scores.lof > 1.8)
length(outliers.lof)


# 3. 类别处理不平衡
y <- factor(data$class)
table(y)

upsampled <- upSample(x = x.scaled, y = y)
table(upsampled$Class)


downsampled <- downSample(x = x.scaled, y = y)
table(downsampled$Class)


############### 3. 模型构建  #######################
# 逻辑回归
mlogit.cls.model <- train(class ~ ., data = train,
                          method = "multinom",
                          trControl = cv.ctrl,
                          trace = FALSE)
print(mlogit.cls.model)

# 支持向量机
svm.cls.model <- train(class ~ ., data = train,
                       method = "svmRadial",
                       preProcess = c("center", "scale"),
                       trControl = cv.ctrl)
print(svm.cls.model)

# 随机森林
rf.cls.model <- train(class ~ ., data = train,
                      method = "rf",
                      trControl = cv.ctrl,
                      ntree = 500)
print(rf.cls.model)


########## 4. 模型评估 ##########
pre.mlogit.train <- predict(mlogit.cls.model)
confusionMatrix(pre.mlogit.train, as.factor(train$class))

pre.mlogit.prob.train <- predict(mlogit.cls.model, newdata = train, type = "prob")
df.train <- data.frame(
  obs = factor(train$class),  
  pred = factor(pre.mlogit.train)  
)
df.train <- cbind(df.train, pre.mlogit.prob.train)
colnames(df.train) <- gsub(" ", "", colnames(df.train))
library(yardstick)
accuracy(df.train, obs, pred)$.estimate
precision(df.train, obs, pred, estimator = "macro")$.estimate
recall(df.train, obs, pred, estimator = "macro")$.estimate
specificity(df.train, obs, pred, estimator = "macro")$.estimate
f_meas(df.train, obs, pred, estimator = "macro")$.estimate
mcc(df.train, obs, pred, estimator = "macro")$.estimate
roc_auc(df.train, obs, d:s, estimator = "macro")$.estimate
pr_auc(df.train, obs, d:s, estimator = "macro")$.estimate


pdf("分类模型ROC.pdf", width = 10, height = 8)
roc_curve(df.train, obs, d, h, o, s) %>% 
  ggplot(aes(1-specificity, sensitivity))+
  geom_line()+
  geom_abline(linetype = 2)+
  facet_wrap(vars(.level))+
  theme_bw() +
  theme(
    plot.title = element_text(size = 18),     # 设置标题字体大小
    axis.title = element_text(size = 18),     # 设置轴标题字体大小
    axis.text = element_text(size = 18),      # 设置坐标轴刻度字体大小
    legend.title = element_text(size = 18),   # 设置图例标题字体大小
    legend.text = element_text(size = 18),     # 设置图例文本字体大小
    strip.text = element_text(size = 18) 
  )
dev.off()

pdf("分类模型PR.pdf", width = 10, height = 8)
pr_curve(df.train, obs, d, h, o, s) %>% 
  ggplot(aes(recall, precision))+
  geom_line() +
  annotate("segment", x = 0, y = 1, xend = 1, yend = 0, 
           linetype = "dashed", color = "gray") + 
  facet_wrap(vars(.level))+
  theme_bw() +
  theme(
    plot.title = element_text(size = 18),     # 设置标题字体大小
    axis.title = element_text(size = 18),     # 设置轴标题字体大小
    axis.text = element_text(size = 18),      # 设置坐标轴刻度字体大小
    legend.title = element_text(size = 18),   # 设置图例标题字体大小
    legend.text = element_text(size = 18),     # 设置图例文本字体大小
    strip.text = element_text(size = 18) 
  )
dev.off()

pre.svm.train <- predict(svm.cls.model)
confusionMatrix(pre.svm.train, as.factor(train$class))

# pre.svm.prob.train <- predict(svm.cls.model, type = "prob")
# df.train <- data.frame(
#   obs = factor(train$class),  
#   pred = factor(pre.svm.train)  
# )
# df.train <- cbind(df.train, pre.svm.prob.train)
# colnames(df.train) <- gsub(" ", "", colnames(df.train))
# library(yardstick)
# accuracy(df.train, obs, pred, estimator = "macro")
# precision(df.train, obs, pred, estimator = "macro")
# recall(df.train, obs, pred, estimator = "macro")
# specificity(df.train, obs, pred, estimator = "macro")
# f_meas(df.train, obs, pred, estimator = "macro")
# mcc(df.train, obs, pred, estimator = "macro")
# roc_auc(df.train, obs, d:s, estimator = "macro")
# pr_auc(df.train, obs, d:s, estimator = "macro")
# 
# roc_curve(df.train, obs, d, h, o, s)%>% 
#   autoplot()



pre.rf.train <- predict(rf.cls.model)
confusionMatrix(pre.rf.train, as.factor(train$class))


pre.rf.prob.train <- predict(rf.cls.model, type = "prob")
df.train <- data.frame(
  obs = factor(train$class),  
  pred = factor(pre.rf.train)  
)
df.train <- cbind(df.train, pre.rf.prob.train)
colnames(df.train) <- gsub(" ", "", colnames(df.train))
library(yardstick)
accuracy(df.train, obs, pred, estimator = "macro")
precision(df.train, obs, pred, estimator = "macro")
recall(df.train, obs, pred, estimator = "macro")
specificity(df.train, obs, pred, estimator = "macro")
f_meas(df.train, obs, pred, estimator = "macro")
mcc(df.train, obs, pred, estimator = "macro")
roc_auc(df.train, obs, d:s, estimator = "macro")
pr_auc(df.train, obs, d:s, estimator = "macro")

roc_curve(df.train, obs, d, h, o, s) %>% 
  autoplot()



############### 6. 结果分析 ###########################

pred.rf.test <- predict(rf.cls.model, newdata = test)
obs.clean  <- trimws(as.character(test$class))
pred.clean <- trimws(as.character(pred.rf.test))
levs <- levels(factor(obs.clean))
test.df <- data.frame(
  obs  = factor(obs.clean,  levels = levs),
  pred = factor(pred.clean, levels = levs)
)

results <- multiClassSummary(data = test.df, lev = levs, model = "rf")
print(round(results, 3))

library(randomForest)
# pdf("随机森林重要性.pdf", width = 6, height = 4)
varImpPlot(rf.cls.model$finalModel, cex.lab = 1.4, cex.axis = 1.4, main = "")
# dev.off()


pred.rf.test <- predict(rf.cls.model, newdata = test)
obs.clean <- trimws(as.character(test$class))
pred.clean <- trimws(as.character(pred.rf.test))
levs <- levels(factor(obs.clean))
test.df <- data.frame(
  obs = factor(obs.clean, levels = levs),
  pred = factor(pred.clean, levels = levs)
)
results <- multiClassSummary(data = test.df, lev = levs, model = "rf")
print(round(results, 3))

# AUC曲线和PR曲线
pre.rf.raw.test <- predict(rf.cls.model, newdata = test, type = "raw")
pre.rf.prob.test <- predict(rf.cls.model, newdata = test, type = "prob")
df.test <- data.frame(
  obs = factor(test$class),  
  pred = factor(pre.rf.raw.test)  
)
df.test <- cbind(df.test, pre.rf.prob.test)
colnames(df.test) <- gsub(" ", "", colnames(df.test))
library(yardstick)
accuracy(df.test, obs, pred)$.estimate
precision(df.test, obs, pred, estimator = "macro")$.estimate
recall(df.test, obs, pred, estimator = "macro")$.estimate
specificity(df.test, obs, pred, estimator = "macro")$.estimate
f_meas(df.test, obs, pred, estimator = "macro")$.estimate
mcc(df.test, obs, pred)$.estimate
roc_auc(df.test, obs, d:s, estimator = "macro")$.estimate
pr_auc(df.test, obs, d:s, estimator = "macro")$.estimate










