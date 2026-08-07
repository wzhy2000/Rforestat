library(caret)
library(tidyverse)

############## 1. 数据加载与划分 ###################

cls.data <- read.csv("ASTER.csv")
str(cls.data)
cls.data$class <- factor(trimws(cls.data$class))

set.seed(123)
cls.train.idx <- createDataPartition(cls.data$class, p = 0.8, list = FALSE)
cls.train <- cls.data[cls.train.idx, ]
cls.test <- cls.data[-cls.train.idx, ]

cat("训练集样本数：", nrow(cls.train), "\n")
cat("测试集样本数：", nrow(cls.test), "\n")
cat("总样本数：", nrow(cls.data), "\n")

############## 2. 样本预处理 #################

# （1）自助法Bootstrap演示
set.seed(123)
cls.bootstrap.samples <- createResample(cls.train$class, times = 3)
str(cls.bootstrap.samples)

cls.boot.train1 <- cls.train[cls.bootstrap.samples[[1]], ]
nrow(cls.boot.train1)

cls.oob.index <- setdiff(
  seq_len(nrow(cls.train)),
  unique(cls.bootstrap.samples[[1]])
)
cls.boot.oob1 <- cls.train[cls.oob.index, ]
nrow(cls.boot.oob1)

# 以下孤立森林和LOF代码仅用于知识点演示，不参与后续模型训练
# （2）孤立森林
library(isotree)
set.seed(123)
cls.sample.predictors <- setdiff(names(cls.train), "class")
cls.train.iso <- cls.train[, cls.sample.predictors, drop = FALSE]
cls.iso.model <- isolation.forest(cls.train.iso, ntrees = 100)
cls.train.iso.scores <- predict(cls.iso.model, cls.train.iso)
cls.train.iso.outliers <- which(cls.train.iso.scores > 0.6)
length(cls.train.iso.outliers)

# （3）局部离群因子LOF
library(Rlof)
cls.x.scaled <- scale(cls.train[, cls.sample.predictors])
cls.lof.scores <- lof(cls.x.scaled, k = 20, cores = 1)
cls.lof.outliers <- which(cls.lof.scores > 1.8)
length(cls.lof.outliers)

# （4）类别不平衡处理演示
cls.y <- cls.train$class
table(cls.y)

set.seed(123)
cls.upsampled <- upSample(
  x = cls.train[, cls.sample.predictors, drop = FALSE],
  y = cls.y
)
table(cls.upsampled$Class)

set.seed(123)
cls.downsampled <- downSample(
  x = cls.train[, cls.sample.predictors, drop = FALSE],
  y = cls.y
)
table(cls.downsampled$Class)

############## 3. 特征预处理 #################

# 1. 缺失值处理
sapply(cls.train, function(x) sum(is.na(x)))
sapply(cls.test, function(x) sum(is.na(x)))

cls.train <- cls.train %>% drop_na()
cls.test <- cls.test %>% drop_na()

# 2. 数值型特征预处理
# （1）Z-score异常值筛查演示
cls.b1.mean <- mean(cls.train$b1, na.rm = TRUE)
cls.b1.sd <- sd(cls.train$b1, na.rm = TRUE)
cls.train.b1.z <- (cls.train$b1 - cls.b1.mean) / cls.b1.sd
cls.test.b1.z <- (cls.test$b1 - cls.b1.mean) / cls.b1.sd

cls.b1.outliers <- cls.train$b1[abs(cls.train.b1.z) > 3]
head(cls.b1.outliers)

cls.train.z.demo <- cls.train[
  abs(cls.train.b1.z) <= 3,
  ,
  drop = FALSE
]

# （2）使用训练集参数进行中心化和标准化
cls.b1.scaled <- scale(cls.train$b1, center = TRUE, scale = TRUE)
cls.b1.center <- attr(cls.b1.scaled, "scaled:center")
cls.b1.scale <- attr(cls.b1.scaled, "scaled:scale")
cls.b1.test.scaled <- scale(
  cls.test$b1,
  center = cls.b1.center,
  scale = cls.b1.scale
)
attributes(cls.b1.scaled)

# （3）使用训练集估计Box-Cox参数
library(e1071)
skewness(cls.train$b1)

cls.b1.boxcox <- BoxCoxTrans(cls.train$b1)
cls.b1.boxcox

cls.b1.train.boxcoxed <- predict(cls.b1.boxcox, cls.train$b1)
cls.b1.test.boxcoxed <- predict(cls.b1.boxcox, cls.test$b1)
head(cls.b1.train.boxcoxed)
skewness(cls.b1.train.boxcoxed)

# 3. 分类变量编码
cls.train$class <- factor(cls.train$class)
cls.test$class <- factor(cls.test$class, levels = levels(cls.train$class))

# 4. 特征选择
cls.train.num <- cls.train[, setdiff(names(cls.train), "class"), drop = FALSE]

# （1）过滤法
cls.nzv <- nearZeroVar(
  cls.train.num,
  freqCut = 20,
  uniqueCut = 10,
  saveMetrics = TRUE
)
cls.nzv

# （2）基于训练集的共线性筛选
cls.cor.matrix <- cor(cls.train.num)
library(corrplot)

# pdf("ASTER 影像数据集特征之间相关性图.pdf", width = 8, height = 6)
gray.cols <- colorRampPalette(
  c("white", "gray50", "black")
)(200)
corrplot(
  cls.cor.matrix,
  order = "hclust",
  method = "circle",
  type = "full",
  col = gray.cols,
  cl.lim = c(-1, 1),
  number.cex = 1,
  tl.cex = 1,
  tl.col = "black",
  cl.cex = 1,
  addgrid.col = "gray70"
)
# dev.off()

cls.high.corr <- findCorrelation(cls.cor.matrix, cutoff = 0.6)
cls.high.corr
cls.removed.predictors <- names(cls.train.num)[cls.high.corr]
cls.predictors <- setdiff(names(cls.train.num), cls.removed.predictors)

cls.train <- cls.train[, c("class", cls.predictors), drop = FALSE]
cls.test <- cls.test[, c("class", cls.predictors), drop = FALSE]
colnames(cls.train)

# 5. 特征降维与提取
library(psych)
KMO(cls.train[, cls.predictors])
psych::cortest.bartlett(cls.train[, cls.predictors])

cls.pca <- prcomp(
  cls.train[, cls.predictors],
  scale. = TRUE,
  center = TRUE
)
cls.train.pca <- predict(cls.pca, newdata = cls.train[, cls.predictors])
cls.test.pca <- predict(cls.pca, newdata = cls.test[, cls.predictors])
cls.train.pca[1:5, 1:5]

############### 4. 重采样设置 #####################

# （1）交叉验证索引
set.seed(123)
cls.cv.train.index <- createFolds(cls.train$class, k = 5, returnTrain = TRUE)
cls.cv.valid.index <- lapply(
  cls.cv.train.index,
  function(idx) setdiff(seq_len(nrow(cls.train)), idx)
)
cls.cv.ctrl <- trainControl(
  method = "cv",
  number = 5,
  index = cls.cv.train.index,
  indexOut = cls.cv.valid.index,
  classProbs = TRUE,
  savePredictions = "final"
)

for (i in seq_along(cls.cv.train.index)) {
  cat(sprintf(
    "第 %d 折：训练集 = %d，验证集 = %d\n",
    i,
    length(cls.cv.train.index[[i]]),
    length(cls.cv.valid.index[[i]])
  ))
}
cat("总样本数：", nrow(cls.train), "\n")

############### 5. 模型构建 #######################

# 多项逻辑回归
set.seed(123)
cls.mlogit.model <- train(
  class ~ .,
  data = cls.train,
  method = "multinom",
  trControl = cls.cv.ctrl,
  trace = FALSE
)
print(cls.mlogit.model)

# 支持向量机
set.seed(123)
cls.svm.model <- train(
  class ~ .,
  data = cls.train,
  method = "svmRadial",
  preProcess = c("center", "scale"),
  trControl = cls.cv.ctrl
)
print(cls.svm.model)

# 随机森林
set.seed(123)
cls.rf.model <- train(
  class ~ .,
  data = cls.train,
  method = "rf",
  trControl = cls.cv.ctrl,
  ntree = 500
)
print(cls.rf.model)

########## 6. 模型评估 ##########

library(yardstick)

cls.df.cv <- cls.mlogit.model$pred %>%
  select(obs, pred, all_of(levels(cls.train$class)))
confusionMatrix(cls.df.cv$pred, cls.df.cv$obs)

accuracy(cls.df.cv, obs, pred)$.estimate
precision(cls.df.cv, obs, pred, estimator = "macro")$.estimate
recall(cls.df.cv, obs, pred, estimator = "macro")$.estimate
specificity(cls.df.cv, obs, pred, estimator = "macro")$.estimate
f_meas(cls.df.cv, obs, pred, estimator = "macro")$.estimate
mcc(cls.df.cv, obs, pred)$.estimate
roc_auc(cls.df.cv, obs, d:s, estimator = "macro")$.estimate
pr_auc(cls.df.cv, obs, d:s, estimator = "macro")$.estimate

cls.get.best <- function(cls.model, cls.name) {
  cls.best <- merge(cls.model$results, cls.model$bestTune)
  
  cls.fold.f1 <- cls.model$pred %>%
    group_by(Resample) %>%
    summarise(
      F1 = yardstick::f_meas_vec(
        truth = obs, estimate = pred, estimator = "macro"
      ),
      .groups = "drop"
    )
  
  data.frame(
    Model = cls.name,
    Accuracy = cls.best$Accuracy,
    Kappa = cls.best$Kappa,
    F1 = mean(cls.fold.f1$F1)
  )
}

cls.cv.results <- bind_rows(
  cls.get.best(cls.mlogit.model, "多类逻辑回归"),
  cls.get.best(cls.svm.model, "支持向量机"),
  cls.get.best(cls.rf.model, "随机森林")
)
print(cls.cv.results)

cls.selected.model <- cls.cv.results$Model[which.max(cls.cv.results$Accuracy)]
cat("按交叉验证Accuracy选择的分类模型：", cls.selected.model, "\n")

pdf("分类模型ROC.pdf", width = 10, height = 8, family = "GB1")
roc_curve(cls.df.cv, obs, d, h, o, s) %>%
  ggplot(aes(1 - specificity, sensitivity)) +
  geom_line() +
  geom_abline(linetype = 2) +
  facet_wrap(vars(.level)) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    strip.background = element_rect(
      fill = "white",
      colour = "black"
    ),
    strip.text = element_text(size = 18, colour = "black"),
    plot.title = element_text(size = 18),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 18),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18)
  )
dev.off()

pdf("分类模型PR.pdf", width = 10, height = 8, family = "GB1")
pr_curve(cls.df.cv, obs, d, h, o, s) %>%
  ggplot(aes(recall, precision)) +
  geom_line() +
  facet_wrap(vars(.level)) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    strip.background = element_rect(
      fill = "white",
      colour = "black"
    ),
    strip.text = element_text(size = 18, colour = "black"),
    plot.title = element_text(size = 18),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 18),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18)
  )
dev.off()

############### 7. 结果分析 ###########################

cls.pre.mlogit.raw.test <- predict(
  cls.mlogit.model,
  newdata = cls.test,
  type = "raw"
)
cls.pre.mlogit.prob.test <- predict(
  cls.mlogit.model,
  newdata = cls.test,
  type = "prob"
)
cls.df.test <- data.frame(
  obs = cls.test$class,
  pred = cls.pre.mlogit.raw.test
)
cls.df.test <- cbind(cls.df.test, cls.pre.mlogit.prob.test)

cls.test.results <- multiClassSummary(
  data = cls.df.test,
  lev = levels(cls.train$class),
  model = "multinom"
)
print(round(cls.test.results, 3))

accuracy(cls.df.test, obs, pred)$.estimate
precision(cls.df.test, obs, pred, estimator = "macro")$.estimate
recall(cls.df.test, obs, pred, estimator = "macro")$.estimate
specificity(cls.df.test, obs, pred, estimator = "macro")$.estimate
f_meas(cls.df.test, obs, pred, estimator = "macro")$.estimate
mcc(cls.df.test, obs, pred)$.estimate
roc_auc(cls.df.test, obs, d:s, estimator = "macro")$.estimate
pr_auc(cls.df.test, obs, d:s, estimator = "macro")$.estimate

library(randomForest)
# pdf("随机森林重要性.pdf", width = 10, height = 6.9, family = "GB1")
par(mar = c(6, 5.5, 4, 2), mgp = c(2, 1, 0))
varImpPlot(
  cls.rf.model$finalModel,
  cex = 2,
  cex.lab = 1,
  main = ""
)
# dev.off()

