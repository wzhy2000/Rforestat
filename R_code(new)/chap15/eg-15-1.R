# remotes::install_github("FoRTExperiment/fortedata", dependencies = TRUE)
library(fortedata)
library(caret)
library(tidyverse)

############## 1. 数据加载与划分 ###################

reg.data <- fd_soil_respiration()
str(reg.data)

reg.data <- reg.data %>%
  select(-date, -timestamp)

# 监督学习要求响应变量已观测；预测变量的缺失值在划分后处理
reg.data <- reg.data %>%
  filter(!is.na(soil_co2_efflux))

# 每个子样地只保留一行划分信息，再使用响应变量中位数维持分层
reg.subplot.data <- reg.data %>%
  group_by(subplot_id) %>%
  summarise(
    reg.stratum = median(soil_co2_efflux),
    .groups = "drop"
  )

set.seed(123)
reg.train.subplot.idx <- createDataPartition(
  reg.subplot.data$reg.stratum,
  p = 0.8,
  list = FALSE,
  groups = 2
)
reg.train.subplots <- reg.subplot.data$subplot_id[reg.train.subplot.idx]
reg.test.subplots <- setdiff(reg.subplot.data$subplot_id, reg.train.subplots)

reg.train <- reg.data %>%
  filter(subplot_id %in% reg.train.subplots)
reg.test <- reg.data %>%
  filter(subplot_id %in% reg.test.subplots)

cat("训练集样本数：", nrow(reg.train), "\n")
cat("测试集样本数：", nrow(reg.test), "\n")
cat("总样本数：", nrow(reg.data), "\n")
cat("训练集子样地数：", n_distinct(reg.train$subplot_id), "\n")
cat("测试集子样地数：", n_distinct(reg.test$subplot_id), "\n")
cat(
  "训练集与测试集重叠子样地数：",
  length(intersect(reg.train.subplots, reg.test.subplots)),
  "\n"
)

############## 2. 样本预处理 ###################

# （1）以subplot_id为单位进行整组Bootstrap
reg.subplot.rows <- split(
  seq_len(nrow(reg.train)),
  reg.train$subplot_id
)

set.seed(123)
reg.bootstrap.subplots <- replicate(
  3,
  sample(
    names(reg.subplot.rows),
    size = length(reg.subplot.rows),
    replace = TRUE
  ),
  simplify = FALSE
)
names(reg.bootstrap.subplots) <- paste0(
  "Resample",
  seq_along(reg.bootstrap.subplots)
)
reg.bootstrap.samples <- lapply(
  reg.bootstrap.subplots,
  function(ids) {
    unlist(reg.subplot.rows[ids], use.names = FALSE)
  }
)
str(reg.bootstrap.subplots)

reg.boot.train1 <- reg.train[
  reg.bootstrap.samples[[1]],
  ,
  drop = FALSE
]
nrow(reg.boot.train1)

reg.oob.subplots <- setdiff(
  names(reg.subplot.rows),
  unique(reg.bootstrap.subplots[[1]])
)
reg.boot.oob1 <- reg.train[
  reg.train$subplot_id %in% reg.oob.subplots,
  ,
  drop = FALSE
]
length(reg.oob.subplots)
nrow(reg.boot.oob1)

# 以下孤立森林和LOF代码仅用于知识点演示，不参与后续模型训练
# （2）孤立森林
library(isotree)
set.seed(123)
reg.sample.predictors <- c("soil_temp", "vwc")
reg.train.iso <- reg.train[, reg.sample.predictors, drop = FALSE] %>%
  drop_na()
reg.iso.model <- isolation.forest(reg.train.iso, ntrees = 100)
reg.train.iso.scores <- predict(reg.iso.model, reg.train.iso)
reg.train.iso.outliers <- which(reg.train.iso.scores > 0.6)
length(reg.train.iso.outliers)

# （3）局部离群因子LOF
library(Rlof)
reg.x.scaled <- scale(reg.train.iso)
reg.lof.scores <- lof(reg.x.scaled, k = 20, cores = 1)
reg.lof.outliers <- which(reg.lof.scores > 2.2)
length(reg.lof.outliers)

############## 3. 特征预处理 ###################

# 1. 缺失值处理
sapply(reg.train, function(x) sum(is.na(x)))
sapply(reg.test, function(x) sum(is.na(x)))

reg.train <- reg.train %>% drop_na()
reg.test <- reg.test %>% drop_na()

# 2. 数值型特征预处理
# （1）IQR异常值筛查演示
reg.q1 <- quantile(reg.train$vwc, 0.25)
reg.q3 <- quantile(reg.train$vwc, 0.75)
reg.iqr <- reg.q3 - reg.q1
reg.lower.bound <- reg.q1 - 1.5 * reg.iqr
reg.upper.bound <- reg.q3 + 1.5 * reg.iqr

reg.outliers.iqr <- reg.train$vwc[
  reg.train$vwc < reg.lower.bound | reg.train$vwc > reg.upper.bound
]
head(reg.outliers.iqr)

reg.train.iqr.demo <- reg.train[
  reg.train$vwc >= reg.lower.bound & reg.train$vwc <= reg.upper.bound,
  ,
  drop = FALSE
]

# （2）使用训练集参数进行中心化和标准化
reg.vwc.scaled <- scale(reg.train$vwc, center = TRUE, scale = TRUE)
reg.vwc.center <- attr(reg.vwc.scaled, "scaled:center")
reg.vwc.scale <- attr(reg.vwc.scaled, "scaled:scale")
reg.vwc.test.scaled <- scale(
  reg.test$vwc,
  center = reg.vwc.center,
  scale = reg.vwc.scale
)
attributes(reg.vwc.scaled)

# （3）使用训练集估计Box-Cox参数
library(e1071)
skewness(reg.train$vwc)

reg.vwc.boxcox <- BoxCoxTrans(reg.train$vwc)
reg.vwc.boxcox

reg.vwc.train.boxcoxed <- predict(reg.vwc.boxcox, reg.train$vwc)
reg.vwc.test.boxcoxed <- predict(reg.vwc.boxcox, reg.test$vwc)
head(reg.vwc.train.boxcoxed)
skewness(reg.vwc.train.boxcoxed)

# 保存分组变量；subplot_id仅用于分组，不作为模型预测变量
reg.train.groups <- reg.train$subplot_id
reg.test.groups <- reg.test$subplot_id

# 3. 分类变量哑变量编码
reg.factor.vars <- c(
  "replicate", "plot", "subplot", "nested_subplot", "run"
)
reg.train[reg.factor.vars] <- lapply(reg.train[reg.factor.vars], factor)
reg.test[reg.factor.vars] <- Map(
  function(x, lev) factor(x, levels = lev),
  reg.test[reg.factor.vars],
  lapply(reg.train[reg.factor.vars], levels)
)

reg.dummies <- dummyVars(
  ~ . - soil_temp - soil_co2_efflux - vwc - subplot_id,
  data = reg.train,
  fullRank = TRUE
)
reg.train.transformed <- predict(reg.dummies, newdata = reg.train)
reg.test.transformed <- predict(reg.dummies, newdata = reg.test)

reg.train <- cbind(
  data.frame(reg.train.transformed),
  soil_temp = reg.train$soil_temp,
  vwc = reg.train$vwc,
  soil_co2_efflux = reg.train$soil_co2_efflux
)
reg.test <- cbind(
  data.frame(reg.test.transformed),
  soil_temp = reg.test$soil_temp,
  vwc = reg.test$vwc,
  soil_co2_efflux = reg.test$soil_co2_efflux
)

# 4. 特征选择
reg.predictors <- c("soil_temp", "vwc")
reg.train.num <- reg.train[, reg.predictors, drop = FALSE]
reg.test.num <- reg.test[, reg.predictors, drop = FALSE]

# （1）过滤法
reg.nzv <- nearZeroVar(
  reg.train.num,
  freqCut = 20,
  uniqueCut = 10,
  saveMetrics = TRUE
)
reg.nzv

# （2）共线性检验
reg.cor.matrix <- cor(reg.train.num)
library(corrplot)

pdf("土壤CO2通量数据集特征之间相关性图.pdf", width = 8, height = 6)
gray.cols <- colorRampPalette(
  c("white", "gray50", "black")
)(200)

corrplot(
  reg.cor.matrix,
  order = "hclust",
  method = "circle",
  type = "full",
  col = gray.cols,
  cl.lim = c(-1, 1),
  addCoef.col = "black",
  number.cex = 1.5,
  tl.cex = 1.5,
  tl.col = "black",
  cl.cex = 1.5,
  addgrid.col = "gray70"
)
dev.off()


reg.high.corr <- findCorrelation(reg.cor.matrix, cutoff = 0.6)
reg.high.corr

# 5. 特征降维与提取
library(psych)
KMO(reg.train.num)
psych::cortest.bartlett(reg.train.num)

reg.pca <- prcomp(reg.train[, reg.predictors], scale. = TRUE, center = TRUE)
reg.pca
reg.test.pca <- predict(reg.pca, newdata = reg.test[, reg.predictors])

############## 4. 重采样设置 #################

# （1）按子样地建立固定的交叉验证索引
set.seed(123)
reg.cv.train.index <- groupKFold(reg.train.groups, k = 5)
reg.cv.valid.index <- lapply(
  reg.cv.train.index,
  function(idx) setdiff(seq_len(nrow(reg.train)), idx)
)

reg.summary <- function(data, lev = NULL, model = NULL) {
  reg.residuals <- data$obs - data$pred
  reg.sst <- sum((data$obs - mean(data$obs))^2)
  c(
    RMSE = sqrt(mean(reg.residuals^2)),
    R.squared = 1 - sum(reg.residuals^2) / reg.sst,
    MAE = mean(abs(reg.residuals))
  )
}

reg.cv.ctrl <- trainControl(
  method = "cv",
  number = length(reg.cv.train.index),
  index = reg.cv.train.index,
  indexOut = reg.cv.valid.index,
  summaryFunction = reg.summary,
  savePredictions = "final"
)

reg.foldid <- integer(nrow(reg.train))
for (i in seq_along(reg.cv.valid.index)) {
  reg.foldid[reg.cv.valid.index[[i]]] <- i
}
stopifnot(all(reg.foldid > 0))

for (i in seq_along(reg.cv.train.index)) {
  cat(sprintf(
    "第 %d 折：训练集 = %d，验证集 = %d，验证子样地 = %d\n",
    i,
    length(reg.cv.train.index[[i]]),
    length(reg.cv.valid.index[[i]]),
    n_distinct(reg.train.groups[reg.cv.valid.index[[i]]])
  ))
  stopifnot(length(intersect(
    unique(reg.train.groups[reg.cv.train.index[[i]]]),
    unique(reg.train.groups[reg.cv.valid.index[[i]]])
  )) == 0)
}
cat("总样本数：", nrow(reg.train), "\n")

################ 5. 模型构建 ################

reg.train$log.efflux <- log1p(reg.train$soil_co2_efflux)
reg.test$log.efflux <- log1p(reg.test$soil_co2_efflux)

# （1）线性回归模型
set.seed(123)
reg.lm.model <- train(
  log.efflux ~ soil_temp + vwc,
  data = reg.train,
  method = "lm",
  trControl = reg.cv.ctrl
)
print(reg.lm.model)

# （2）岭回归模型
library(glmnet)
reg.x.train.ridge <- as.matrix(reg.train[, reg.predictors])
reg.y.train.ridge <- reg.train$log.efflux
reg.ridge.model <- cv.glmnet(
  reg.x.train.ridge,
  reg.y.train.ridge,
  alpha = 0,
  foldid = reg.foldid,
  keep = TRUE
)
reg.ridge.fold.mse <- sapply(
  seq_along(reg.ridge.model$lambda),
  function(j) {
    vapply(
      reg.cv.valid.index,
      function(idx) {
        mean(
          (
            reg.y.train.ridge[idx] -
              reg.ridge.model$fit.preval[idx, j]
          )^2
        )
      },
      numeric(1)
    )
  }
)
reg.ridge.cv.mean <- colMeans(reg.ridge.fold.mse)
reg.ridge.cv.se <- apply(reg.ridge.fold.mse, 2, sd) /
  sqrt(nrow(reg.ridge.fold.mse))
reg.ridge.lambda.index <- which.min(reg.ridge.cv.mean)
reg.ridge.lambda.min <- reg.ridge.model$lambda[reg.ridge.lambda.index]
reg.ridge.lambda.1se.index <- which(
  reg.ridge.cv.mean <=
    reg.ridge.cv.mean[reg.ridge.lambda.index] +
      reg.ridge.cv.se[reg.ridge.lambda.index]
)[1]
reg.ridge.lambda.1se <- reg.ridge.model$lambda[
  reg.ridge.lambda.1se.index
]
reg.ridge.cv.summary <- data.frame(
  Lambda = c(reg.ridge.lambda.min, reg.ridge.lambda.1se),
  Index = c(reg.ridge.lambda.index, reg.ridge.lambda.1se.index),
  Measure = reg.ridge.cv.mean[
    c(reg.ridge.lambda.index, reg.ridge.lambda.1se.index)
  ],
  SE = reg.ridge.cv.se[
    c(reg.ridge.lambda.index, reg.ridge.lambda.1se.index)
  ],
  Nonzero = reg.ridge.model$nzero[
    c(reg.ridge.lambda.index, reg.ridge.lambda.1se.index)
  ]
)
rownames(reg.ridge.cv.summary) <- c("min", "1se")
par(mar = c(5, 5, 4, 2))
plot(
  log(reg.ridge.model$lambda),
  reg.ridge.cv.mean,
  type = "l",
  xlab = expression(log(lambda)),
  ylab = "Mean MSE across folds",
  cex.lab = 2.2,
  cex.axis = 2.2
)
abline(
  v = log(c(reg.ridge.lambda.min, reg.ridge.lambda.1se)),
  lty = 2
)
print(reg.ridge.cv.summary)

# （3）非线性回归模型
reg.nls.model <- nls(
  log.efflux ~ a * (1 - exp(-b * soil_temp - c * vwc)),
  start = c(a = 1, b = 0.1, c = 0.01),
  data = reg.train
)
summary(reg.nls.model)

reg.nls.oof <- rep(NA_real_, nrow(reg.train))
for (i in seq_along(reg.cv.train.index)) {
  reg.nls.fold.model <- nls(
    log.efflux ~ a * (1 - exp(-b * soil_temp - c * vwc)),
    start = c(a = 1, b = 0.1, c = 0.01),
    data = reg.train[reg.cv.train.index[[i]], ]
  )
  reg.nls.oof[reg.cv.valid.index[[i]]] <- predict(
    reg.nls.fold.model,
    newdata = reg.train[reg.cv.valid.index[[i]], ]
  )
}

# （4）随机森林回归
library(randomForest)
set.seed(123)
reg.rf.model <- train(
  log.efflux ~ soil_temp + vwc,
  data = reg.train,
  method = "rf",
  trControl = reg.cv.ctrl
)
print(reg.rf.model)

################# 6. 模型评估 ####################

evaluate.regression <- function(y.true, y.pred) {
  if (length(y.true) != length(y.pred)) {
    stop("y.true 和 y.pred 必须长度一致")
  }
  if (any(!is.finite(y.true)) || any(!is.finite(y.pred))) {
    stop("y.true 和 y.pred 必须为有限数值")
  }
  if (length(y.true) < 2 || isTRUE(all.equal(var(y.true), 0))) {
    stop("y.true 必须包含至少两个不同的有限观测值")
  }

  residuals <- y.true - y.pred
  mse <- mean(residuals^2)
  rmse <- sqrt(mse)
  mae <- mean(abs(residuals))
  ss.total <- sum((y.true - mean(y.true))^2)
  ss.res <- sum(residuals^2)
  r.squared <- 1 - ss.res / ss.total

  data.frame(
    MSE = mse,
    RMSE = rmse,
    MAE = mae,
    R.squared = r.squared
  )
}

reg.lm.oof <- rep(NA_real_, nrow(reg.train))
reg.lm.oof[reg.lm.model$pred$rowIndex] <- reg.lm.model$pred$pred

reg.ridge.oof <- reg.ridge.model$fit.preval[, reg.ridge.lambda.index]

reg.rf.oof <- rep(NA_real_, nrow(reg.train))
reg.rf.oof[reg.rf.model$pred$rowIndex] <- reg.rf.model$pred$pred

reg.model.oof <- list(
  "线性回归" = reg.lm.oof,
  "岭回归" = reg.ridge.oof,
  "非线性回归" = reg.nls.oof,
  "随机森林" = reg.rf.oof
)

reg.cv.fold.results <- bind_rows(lapply(
  names(reg.model.oof),
  function(model.name) {
    bind_rows(lapply(
      seq_along(reg.cv.valid.index),
      function(i) {
        idx <- reg.cv.valid.index[[i]]
        cbind(
          Model = model.name,
          Fold = i,
          evaluate.regression(
            reg.train$log.efflux[idx],
            reg.model.oof[[model.name]][idx]
          )
        )
      }
    ))
  }
))

reg.cv.results <- reg.cv.fold.results %>%
  group_by(Model) %>%
  summarise(
    across(c(MSE, RMSE, MAE, R.squared), mean),
    .groups = "drop"
  ) %>%
  slice(match(names(reg.model.oof), Model)) %>%
  as.data.frame()

stopifnot(
  isTRUE(all.equal(
    reg.cv.results$RMSE[reg.cv.results$Model == "线性回归"],
    mean(reg.lm.model$resample$RMSE)
  )),
  isTRUE(all.equal(
    reg.cv.results$RMSE[reg.cv.results$Model == "随机森林"],
    mean(reg.rf.model$resample$RMSE)
  ))
)
print(reg.cv.results)

reg.selected.model <- reg.cv.results$Model[which.min(reg.cv.results$RMSE)]
cat("按分组交叉验证RMSE选择的回归模型：", reg.selected.model, "\n")

stopifnot(reg.selected.model == "非线性回归")
reg.pre.selected.train <- predict(reg.nls.model, newdata = reg.train)

################# 7. 参数估计 ########################

reg.coefficients <- coef(reg.lm.model$finalModel)
print(reg.coefficients)

############### 8. 结果分析 ###########################

reg.pre.selected.test <- predict(reg.nls.model, newdata = reg.test)
evaluate.regression(reg.test$log.efflux, reg.pre.selected.test)

reg.residuals.train <- reg.train$log.efflux - reg.pre.selected.train
pdf("CO2通量-土壤体积含水量train.pdf", width = 10, height = 6, family = "GB1")
par(mar = c(5, 6, 4, 2))
plot(
  reg.train$vwc,
  reg.residuals.train,
  xlab = "土壤体积含水量",
  ylab = "训练集残差",
  cex.lab = 2.2,
  cex.axis = 2.2
)
dev.off()

reg.residuals.test <- reg.test$log.efflux - reg.pre.selected.test
pdf("CO2通量-土壤体积含水量test.pdf", width = 10, height = 6, family = "GB1")
par(mar = c(5, 6, 4, 2))
plot(
  reg.test$vwc,
  reg.residuals.test,
  xlab = "土壤体积含水量",
  ylab = "测试集残差",
  cex.lab = 2.2,
  cex.axis = 2.2
)
dev.off()

reg.coef.summary <- coef(reg.lm.model$finalModel)
print(reg.coef.summary)

pdf("CO2训练集残差图.pdf", width = 10, height = 6, family = "GB1")
par(mar = c(5, 6, 4, 2))
plot(
  reg.pre.selected.train,
  reg.residuals.train,
  xlab = "土壤CO2通量预测值（log1p尺度）",
  ylab = "残差",
  cex.lab = 2.25,
  cex.axis = 2.25
)
abline(h = 0, col = "red", lty = 2)
dev.off()

pdf("CO2测试集残差图.pdf", width = 10, height = 6, family = "GB1")
par(mar = c(5, 6, 4, 2))
plot(
  reg.pre.selected.test,
  reg.residuals.test,
  xlab = "土壤CO2通量预测值（log1p尺度）",
  ylab = "残差",
  cex.lab = 2.25,
  cex.axis = 2.25
)
abline(h = 0, col = "red", lty = 2)
dev.off()

pdf("CO2训练集真实值与预测值散点图.pdf", width = 10, height = 6, family = "GB1")
par(mar = c(5, 6, 4, 2))
plot(
  reg.train$log.efflux,
  reg.pre.selected.train,
  xlab = "真实值（log.efflux）",
  ylab = "预测值（log.efflux）",
  pch = 20,
  col = "black",
  cex = 1,
  cex.lab = 2.25,
  cex.axis = 2.25
)
abline(0, 1, col = "red", lwd = 2, lty = 2)
dev.off()

pdf("CO2测试集真实值与预测值散点图.pdf", width = 10, height = 6, family = "GB1")
par(mar = c(5, 6, 4, 2))
plot(
  reg.test$log.efflux,
  reg.pre.selected.test,
  xlab = "真实值（log.efflux）",
  ylab = "预测值（log.efflux）",
  pch = 20,
  col = "black",
  cex = 1,
  cex.lab = 2.25,
  cex.axis = 2.25
)
abline(0, 1, col = "red", lwd = 2, lty = 2)
dev.off()

print(reg.rf.model$resample)
reg.resample.long <- reg.rf.model$resample %>%
  pivot_longer(cols = c(RMSE, R.squared, MAE), names_to = "Metric", values_to = "Value") %>%
  mutate(Metric = recode(Metric, "R.squared" = "R²"))


pdf("交叉验证结果箱线图.pdf", width = 10, height = 6, family = "GB1")
ggplot(reg.resample.long, aes(x = Metric, y = Value)) +
  geom_boxplot(fill = "white", color = "black", linewidth = 1, width = 0.5) +
  geom_jitter(width = 0.08, size = 3, color = "black", alpha = 0.7) +
  facet_wrap(~Metric, scales = "free_y", nrow = 1, strip.position = "bottom") +
  labs(x = "", y = "") +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", colour = "black"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 26, color = "black"),
    strip.text = element_text(size = 26, color = "black"),
    strip.background = element_blank(),
    strip.placement = "outside"
  )
dev.off()

