library(brms)
library(Rcpp)
library(forestat)
data(birch)
birch.cross <- birch[, c("CW", "D", "SD", "CLR", "PLOT")]

birch.cross$PLOT <- as.factor(birch.cross$PLOT)
set.seed(123)
n <- nrow(birch.cross)
idx.train <- sample(1:n, size = 0.8 * n)
idx.test <- setdiff(1:n, idx.train)
birch.train <- birch.cross[idx.train, ]
birch.test <- birch.cross[idx.test, ]

# 手动计算 R^2
R2 <- function(y_true, y_pred) {
  ss_res <- sum((y_true - y_pred)^2)  # 残差平方和
  ss_tot <- sum((y_true - mean(y_true))^2)  # 总平方和
  return(1 - ss_res / ss_tot)  # 计算 R^2
}


############ 1  实验数据为冬奥核心区的birch数据，实验10.1summary()函数得到的初始值，a1具有样地间的随机效应 ###############
set.seed(123)
prior.cross <- prior(normal(4.397388, 0.5), nlpar = "a1") +
  prior(normal(1.734067, 0.5), nlpar = "a2") +
  prior(normal(2.462342, 0.2), nlpar = "b1") +
  prior(normal(0.130148, 0.1), nlpar = "c1") +
  prior(normal(-0.000005, 0.01), nlpar = "c2")

# 实验10.1设置了a1具有样地的随机效应，因此a1 ~ 1 + (1 | PLOT)
model.test1 <- brm(bf(CW ~  (a1 + a2 * CLR) / (1 + b1 * exp(-(c1 + c2 * SD) * D)), 
                    a1 ~ 1 + (1 | PLOT), a2 ~ 1, b1 ~ 1, c1 ~ 1, c2 ~ 1, nl = TRUE), 
                   data = birch.train, prior = prior.cross, 
                   cores = 4, iter = 5000)

# 使用 allow_new_levels = TRUE，brms 会将新分组的随机效应视为 0（即假设新分组的随机效应均值为 0，而不是拒绝预测）
# 如果新数据 newdata 里可能有未见过的分组水平（如新的 PLOT 值），并且你希望 brms 继续进行预测，而不是报错，就需要 allow_new_levels = TRUE。
pre.train.test1 <- predict(model.test1, newdata = birch.train,  allow_new_levels = TRUE)[,1]
pre.test.test1 <- predict(model.test1, newdata = birch.test, allow_new_levels = TRUE)[, 1]


FittingEvaluationIndex(pre.train.test1, birch.train[, c("CW")])
FittingEvaluationIndex(pre.test.test1, birch.test[, c("CW")])


res.train.test1 <- birch.train[, c("CW")] - pre.train.test1
par(mar = c(5, 5, 4, 2), mgp = c(3.5, 1, 0))
plot(pre.train.test1, res.train.test1, xlab = "拟合值", 
     ylab = "训练集残差", pch = 16, col = "black", cex = 1, 
     cex.lab = 2.2, cex.axis = 2.2)
abline(h = 0, col = "red")

res.test.test1 <- birch.test[, c("CW")] - pre.test.test1
par(mar = c(5, 5, 4, 2), mgp = c(3.5, 1, 0))
plot(pre.test.test1, res.test.test1, xlab = "拟合值", 
     ylab = "测试集残差", pch = 16, col = "black", cex = 1, 
     cex.lab = 2.2, cex.axis = 2.2)
abline(h = 0, col = "red")

# 训练集 R^2
r2_train.test1 <- R2(birch.train[, c("CW")], pre.train.test1)

# 测试集 R^2
r2_test.test1 <- R2(birch.test[, c("CW")], pre.test.test1)

# 输出结果
cat("Training R^2:", r2_train.test1, "\n")
cat("Testing R^2:", r2_test.test1, "\n")


################## 2  实验数据为冬奥核心区的birch数据，粒子群算法得到的初始值（粒子群算法中设置随机效应，在function()函数中实现） ##############
set.seed(123)
prior.cross <- prior(normal(7.34766422, 0.5), nlpar = "a1") +
  prior(normal(2.60303387, 0.5), nlpar = "a2") +
  prior(normal(1.54924001, 0.2), nlpar = "b1") +
  prior(normal(-0.1742399, 0.1), nlpar = "c1") +
  prior(normal(-2.79285443, 0.01), nlpar = "c2")

# 设置a1具有随机效应
model.test2 <- brm(bf(CW ~  (a1 + a2 * CLR) / (1 + b1 * exp(-(c1 + c2 * SD) * D)), 
                      a1 ~ 1 + (1 | PLOT), a2 ~ 1, b1 ~ 1, c1 ~ 1, c2 ~ 1, nl = TRUE), 
                   data = birch.train, prior = prior.cross, 
                   cores = 4, iter = 5000)

# 使用 allow_new_levels = TRUE，brms 会将新分组的随机效应视为 0（即假设新分组的随机效应均值为 0，而不是拒绝预测）
# 如果新数据 newdata 里可能有未见过的分组水平（如新的 PLOT 值），并且你希望 brms 继续进行预测，而不是报错，就需要 allow_new_levels = TRUE。
pre.train.test2 <- predict(model.test2, newdata = birch.train,  allow_new_levels = TRUE)[,1]
pre.test.test2 <- predict(model.test2, newdata = birch.test, allow_new_levels = TRUE)[, 1]


FittingEvaluationIndex(pre.train.test2, birch.train[, c("CW")])
FittingEvaluationIndex(pre.test.test2, birch.test[, c("CW")])


res.train.test2 <- birch.train[, c("CW")] - pre.train.test2
par(mar = c(5, 5, 4, 2), mgp = c(3.5, 1, 0))
plot(pre.train.test2, res.train.test2, xlab = "拟合值", 
     ylab = "训练集残差", pch = 16, col = "black", cex = 1, 
     cex.lab = 2.2, cex.axis = 2.2)
abline(h = 0, col = "red")

res.test.test2 <- birch.test[, c("CW")] - pre.test.test2
par(mar = c(5, 5, 4, 2), mgp = c(3.5, 1, 0))
plot(pre.test.test2, res.test.test2, xlab = "拟合值", 
     ylab = "测试集残差", pch = 16, col = "black", cex = 1, 
     cex.lab = 2.2, cex.axis = 2.2)
abline(h = 0, col = "red")


# 训练集 R^2
r2_train.test2 <- R2(birch.train[, c("CW")], pre.train.test2)

# 测试集 R^2
r2_test.test2 <- R2(birch.test[, c("CW")], pre.test.test2)

# 输出结果
cat("Training R^2:", r2_train.test2, "\n")
cat("Testing R^2:", r2_test.test2, "\n")




####################### 3 实验数据为冬奥核心区的birch数据，粒子群算法得到的初始值（粒子群算法中未设置随机效应，因此brm中也未设置随机效应） ############
set.seed(123)
prior.cross <- prior(normal(2.48499087, 0.5), nlpar = "a1") +
  prior(normal(1.69754479, 0.5), nlpar = "a2") +
  prior(normal(-0.61805166, 0.2), nlpar = "b1") +
  prior(normal(1.28425484, 0.1), nlpar = "c1") +
  prior(normal(0.67796125, 0.01), nlpar = "c2")

# 未设置随机效应
model.test3 <- brm(bf(CW ~  (a1 + a2 * CLR) / (1 + b1 * exp(-(c1 + c2 * SD) * D)), 
                      a1 ~ 1, a2 ~ 1, b1 ~ 1, c1 ~ 1, c2 ~ 1, nl = TRUE), 
                   data = birch.train, prior = prior.cross, 
                   cores = 4, iter = 5000)

# 使用 allow_new_levels = TRUE，brms 会将新分组的随机效应视为 0（即假设新分组的随机效应均值为 0，而不是拒绝预测）
# 如果新数据 newdata 里可能有未见过的分组水平（如新的 PLOT 值），并且你希望 brms 继续进行预测，而不是报错，就需要 allow_new_levels = TRUE。
pre.train.test3 <- predict(model.test3, newdata = birch.train,  allow_new_levels = TRUE)[,1]
pre.test.test3 <- predict(model.test3, newdata = birch.test, allow_new_levels = TRUE)[, 1]


FittingEvaluationIndex(pre.train.test3, birch.train[, c("CW")])
FittingEvaluationIndex(pre.test.test3, birch.test[, c("CW")])


res.train.test3 <- birch.train[, c("CW")] - pre.train.test3
par(mar = c(5, 5, 4, 2), mgp = c(3.5, 1, 0))
plot(pre.train.test3, res.train.test3, xlab = "拟合值", 
     ylab = "训练集残差", pch = 16, col = "black", cex = 1, 
     cex.lab = 2.2, cex.axis = 2.2)
abline(h = 0, col = "red")

res.test.test3 <- birch.test[, c("CW")] - pre.test.test3
par(mar = c(5, 5, 4, 2), mgp = c(3.5, 1, 0))
plot(pre.test.test3, res.test.test3, xlab = "拟合值", 
     ylab = "测试集残差", pch = 16, col = "black", cex = 1, 
     cex.lab = 2.2, cex.axis = 2.2)
abline(h = 0, col = "red")


# 训练集 R^2
r2_train.test3 <- R2(birch.train[, c("CW")], pre.train.test3)

# 测试集 R^2
r2_test.test3 <- R2(birch.test[, c("CW")], pre.test.test3)

# 输出结果
cat("Training R^2:", r2_train.test3, "\n")
cat("Testing R^2:", r2_test.test3, "\n")



############ 4 数据来自实验10.2“lys-bh.CSV”，并使用实验10.2的summary()函数得到的初始值，a1具有区组和样地的随机效应 ##############

data <- read.csv("lys-bh.CSV", sep = ",")
data$PLOT <- as.factor(data$PLOT)
set.seed(123)
n <- nrow(data)
idx.train <- sample(1:n, size = 0.8 * n)
idx.test <- setdiff(1:n, idx.train)
data.train <- data[idx.train, ]
data.test <- data[idx.test, ]

prior.cross <- prior(normal(5.545833, 0.5), nlpar = "a1") +
  prior(normal(1.640592, 0.5), nlpar = "a2") +
  prior(normal(2.814719, 0.2), nlpar = "b1") +
  prior(normal(0.075150 , 0.1), nlpar = "c1") +
  prior(normal(-0.000001, 0.01), nlpar = "c2")

# 实验10.2设置了a1具有区组和样地的随机效应，因此a1 ~ 1 + (1 | BLOCK / PLOT)
model.test4 <- brm(bf(CW ~  (a1 + a2 * CLR) / (1 + b1 * exp(-(c1 + c2 * SD) * D)), 
                a1 ~ 1 + (1 | BLOCK / PLOT), a2 ~ 1, b1 ~ 1, c1 ~ 1, c2 ~ 1, nl = TRUE), 
             data = data.train, prior = prior.cross, 
             cores = 4, iter = 5000)

# 使用 allow_new_levels = TRUE，brms 会将新分组的随机效应视为 0（即假设新分组的随机效应均值为 0，而不是拒绝预测）
# 如果新数据 newdata 里可能有未见过的分组水平（如新的 PLOT 值），并且你希望 brms 继续进行预测，而不是报错，就需要 allow_new_levels = TRUE。
pre.train.test4 <- predict(model.test4, newdata = data.train,  allow_new_levels = TRUE)[,1]
pre.test.test4 <- predict(model.test4, newdata = data.test, allow_new_levels = TRUE)[, 1]


FittingEvaluationIndex(pre.train.test4, data.train[, c("CW")])
FittingEvaluationIndex(pre.test.test4, data.test[, c("CW")])

res.train.test4 <- data.train[, c("CW")] - pre.train.test4
par(mar = c(5, 5, 4, 2), mgp = c(3.5, 1, 0))
plot(pre.train.test4, res.train.test4, xlab = "拟合值", 
     ylab = "训练集残差", pch = 16, col = "black", cex = 1, 
     cex.lab = 2.2, cex.axis = 2.2)
abline(h = 0, col = "red")

res.test.test4 <- data.test[, c("CW")] - pre.test.test4
par(mar = c(5, 5, 4, 2), mgp = c(3.5, 1, 0))
plot(pre.test.test4, res.test.test4, xlab = "拟合值", 
     ylab = "测试集残差", pch = 16, col = "black", cex = 1, 
     cex.lab = 2.2, cex.axis = 2.2)
abline(h = 0, col = "red")

# 训练集 R^2
r2_train.test4 <- R2(data.train[, c("CW")], pre.train.test4)

# 测试集 R^2
r2_test.test4 <- R2(data.test[, c("CW")], pre.test.test4)

# 输出结果
cat("Training R^2:", r2_train.test4, "\n")
cat("Testing R^2:", r2_test.test4, "\n")



###################### 5 数据来自实验10.2“lys-bh.CSV”，仅设置a1的样地随机效应，利用nlme模型得到的初始值 ###############
prior.cross <- prior(normal(6.207130, 0.5), nlpar = "a1") +
  prior(normal(2.775374, 0.5), nlpar = "a2") +
  prior(normal(3.026681, 0.2), nlpar = "b1") +
  prior(normal(0.064003 , 0.1), nlpar = "c1") +
  prior(normal(-0.000012, 0.01), nlpar = "c2")

# 设置了a1具有样地的随机效应，因此a1 ~ 1 + (1 | PLOT)
model.test5 <- brm(bf(CW ~  (a1 + a2 * CLR) / (1 + b1 * exp(-(c1 + c2 * SD) * D)), 
                      a1 ~ 1 + (1 | PLOT), a2 ~ 1, b1 ~ 1, c1 ~ 1, c2 ~ 1, nl = TRUE), 
                   data = data.train, prior = prior.cross, 
                   cores = 4, iter = 5000)

# 使用 allow_new_levels = TRUE，brms 会将新分组的随机效应视为 0（即假设新分组的随机效应均值为 0，而不是拒绝预测）
# 如果新数据 newdata 里可能有未见过的分组水平（如新的 PLOT 值），并且你希望 brms 继续进行预测，而不是报错，就需要 allow_new_levels = TRUE。
pre.train.test5 <- predict(model.test5, newdata = data.train,  allow_new_levels = TRUE)[,1]
pre.test.test5 <- predict(model.test5, newdata = data.test, allow_new_levels = TRUE)[, 1]


FittingEvaluationIndex(pre.train.test5, data.train[, c("CW")])
FittingEvaluationIndex(pre.test.test5, data.test[, c("CW")])

res.train.test5 <- data.train[, c("CW")] - pre.train.test5
par(mar = c(5, 5, 4, 2), mgp = c(3.5, 1, 0))
plot(pre.train.test5, res.train.test5, xlab = "拟合值", 
     ylab = "训练集残差", pch = 16, col = "black", cex = 1, 
     cex.lab = 2.2, cex.axis = 2.2)
abline(h = 0, col = "red")

res.test.test5 <- data.test[, c("CW")] - pre.test.test5
par(mar = c(5, 5, 4, 2), mgp = c(3.5, 1, 0))
plot(pre.test.test5, res.test.test5, xlab = "拟合值", 
     ylab = "测试集残差", pch = 16, col = "black", cex = 1, 
     cex.lab = 2.2, cex.axis = 2.2)
abline(h = 0, col = "red")

# 训练集 R^2
r2_train.test5 <- R2(data.train[, c("CW")], pre.train.test5)

# 测试集 R^2
r2_test.test5 <- R2(data.test[, c("CW")], pre.test.test5)

# 输出结果
cat("Training R^2:", r2_train.test5, "\n")
cat("Testing R^2:", r2_test.test5, "\n")


############## 6 数据来自实验10.2“lys-bh.CSV”，粒子群算法得到的初始值，a1具有样地随机效应 #################
prior.cross <- prior(normal(21.21440892, 0.5), nlpar = "a1") +
  prior(normal(9.90055368, 0.5), nlpar = "a2") +
  prior(normal(6.17432094 , 0.2), nlpar = "b1") +
  prior(normal(0.99140013 , 0.1), nlpar = "c1") +
  prior(normal(-2.98959899, 0.01), nlpar = "c2")

# 实验10.2设置了a1具有样地的随机效应，因此a1 ~ 1 + (1 |  PLOT)
model.test6 <- brm(bf(CW ~  (a1 + a2 * CLR) / (1 + b1 * exp(-(c1 + c2 * SD) * D)), 
                      a1 ~ 1 + (1 | PLOT), a2 ~ 1, b1 ~ 1, c1 ~ 1, c2 ~ 1, nl = TRUE), 
                   data = data.train, prior = prior.cross, 
                   cores = 4, iter = 5000)

# 使用 allow_new_levels = TRUE，brms 会将新分组的随机效应视为 0（即假设新分组的随机效应均值为 0，而不是拒绝预测）
# 如果新数据 newdata 里可能有未见过的分组水平（如新的 PLOT 值），并且你希望 brms 继续进行预测，而不是报错，就需要 allow_new_levels = TRUE。
pre.train.test6 <- predict(model.test6, newdata = data.train,  allow_new_levels = TRUE)[,1]
pre.test.test6 <- predict(model.test6, newdata = data.test, allow_new_levels = TRUE)[, 1]


FittingEvaluationIndex(pre.train.test6, data.train[, c("CW")])
FittingEvaluationIndex(pre.test.test6, data.test[, c("CW")])

res.train.test6 <- data.train[, c("CW")] - pre.train.test6
par(mar = c(5, 5, 4, 2), mgp = c(3.5, 1, 0))
plot(pre.train.test6, res.train.test6, xlab = "拟合值", 
     ylab = "训练集残差", pch = 16, col = "black", cex = 1, 
     cex.lab = 2.2, cex.axis = 2.2)
abline(h = 0, col = "red")

res.test.test6 <- data.test[, c("CW")] - pre.test.test6
par(mar = c(5, 5, 4, 2), mgp = c(3.5, 1, 0))
plot(pre.test.test6, res.test.test6, xlab = "拟合值", 
     ylab = "测试集残差", pch = 16, col = "black", cex = 1, 
     cex.lab = 2.2, cex.axis = 2.2)
abline(h = 0, col = "red")

# 训练集 R^2
r2_train.test6 <- R2(data.train[, c("CW")], pre.train.test6)

# 测试集 R^2
r2_test.test6 <- R2(data.test[, c("CW")], pre.test.test6)

# 输出结果
cat("Training R^2:", r2_train.test6, "\n")
cat("Testing R^2:", r2_test.test6, "\n")


###################### 7 数据来自实验10.2“lys-bh.CSV”，粒子群算法得到的初始值，无随机效应 ############
prior.cross <- prior(normal(2.34860084, 0.5), nlpar = "a1") +
  prior(normal(2.42216462, 0.5), nlpar = "a2") +
  prior(normal(0.07151957, 0.2), nlpar = "b1") +
  prior(normal(-1.14881981, 0.1), nlpar = "c1") +
  prior(normal(1.60014887, 0.01), nlpar = "c2")

# a1无随机效应
model.test7 <- brm(bf(CW ~  (a1 + a2 * CLR) / (1 + b1 * exp(-(c1 + c2 * SD) * D)), 
                      a1 ~ 1, a2 ~ 1, b1 ~ 1, c1 ~ 1, c2 ~ 1, nl = TRUE), 
                   data = data.train, prior = prior.cross, 
                   cores = 4, iter = 5000)

# 使用 allow_new_levels = TRUE，brms 会将新分组的随机效应视为 0（即假设新分组的随机效应均值为 0，而不是拒绝预测）
# 如果新数据 newdata 里可能有未见过的分组水平（如新的 PLOT 值），并且你希望 brms 继续进行预测，而不是报错，就需要 allow_new_levels = TRUE。
pre.train.test7 <- predict(model.test7, newdata = data.train,  allow_new_levels = TRUE)[,1]
pre.test.test7 <- predict(model.test7, newdata = data.test, allow_new_levels = TRUE)[, 1]


FittingEvaluationIndex(pre.train.test7, data.train[, c("CW")])
FittingEvaluationIndex(pre.test.test7, data.test[, c("CW")])

res.train.test7 <- data.train[, c("CW")] - pre.train.test7
par(mar = c(5, 5, 4, 2), mgp = c(3.5, 1, 0))
plot(pre.train.test7, res.train.test7, xlab = "拟合值", 
     ylab = "训练集残差", pch = 16, col = "black", cex = 1, 
     cex.lab = 2.2, cex.axis = 2.2)
abline(h = 0, col = "red")

res.test.test7 <- data.test[, c("CW")] - pre.test.test7
par(mar = c(5, 5, 4, 2), mgp = c(3.5, 1, 0))
plot(pre.test.test7, res.test.test7, xlab = "拟合值", 
     ylab = "测试集残差", pch = 16, col = "black", cex = 1, 
     cex.lab = 2.2, cex.axis = 2.2)
abline(h = 0, col = "red")

# 训练集 R^2
r2_train.test7 <- R2(data.train[, c("CW")], pre.train.test7)

# 测试集 R^2
r2_test.test7 <- R2(data.test[, c("CW")], pre.test.test7)

# 输出结果
cat("Training R^2:", r2_train.test7, "\n")
cat("Testing R^2:", r2_test.test7, "\n")


# 保存.RData文件
save.image(file = "test.RData")