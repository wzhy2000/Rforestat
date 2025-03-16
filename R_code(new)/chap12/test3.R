# 实验数据为冬奥核心区的birch数据，粒子群算法得到的初始值（粒子群算法中未设置随机效应，，因此brm中也未设置随机效应）
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

prior.cross <- prior(normal(4.397388, 2), nlpar = "a1") +
  prior(normal(1.734067, 2), nlpar = "a2") +
  prior(normal(2.462342, 2), nlpar = "b1") +
  prior(normal(0.130148, 2), nlpar = "c1") +
  prior(normal(-0.000005, 0.01), nlpar = "c2")


model <- brm(bf(CW ~  (a1 + a2 * CLR) / (1 + b1 * exp(-(c1 + c2 * SD) * D)), 
                a1 ~ 1 + (1 | PLOT), a2 ~ 1, b1 ~ 1, c1 ~ 1, c2 ~ 1, nl = TRUE), 
             data = birch.cross[idx.train,], prior = prior.cross, 
             cores = 4, iter = 5000)

# 使用 allow_new_levels = TRUE，brms 会将新分组的随机效应视为 0（即假设新分组的随机效应均值为 0，而不是拒绝预测）
# 如果新数据 newdata 里可能有未见过的分组水平（如新的 PLOT 值），并且你希望 brms 继续进行预测，而不是报错，就需要 allow_new_levels = TRUE。
pre.train <- predict(model, newdata = birch.cross[idx.train,],  allow_new_levels = TRUE)[,1]
pre.test <- predict(model, newdata = birch.cross[idx.test,], allow_new_levels = TRUE)[, 1]


FittingEvaluationIndex(pre.train, birch.cross[idx.train, 1])
FittingEvaluationIndex(pre.test, birch.cross[idx.test, 1])


res.train <- birch.cross[idx.train, 1] - pre.train
par(mar = c(5, 5, 4, 2), mgp = c(3.5, 1, 0))
plot(pre.train, res.train, xlab = "拟合值", 
     ylab = "训练集残差", pch = 16, col = "black", cex = 1, 
     cex.lab = 2.2, cex.axis = 2.2)
abline(h = 0, col = "red")

res.test <- birch.cross[idx.test, 1] - pre.test
par(mar = c(5, 5, 4, 2), mgp = c(3.5, 1, 0))
plot(pre.test, res.test, xlab = "拟合值", 
     ylab = "测试集残差", pch = 16, col = "black", cex = 1, 
     cex.lab = 2.2, cex.axis = 2.2)
abline(h = 0, col = "red")



# 手动计算 R^2
R2 <- function(y_true, y_pred) {
  ss_res <- sum((y_true - y_pred)^2)  # 残差平方和
  ss_tot <- sum((y_true - mean(y_true))^2)  # 总平方和
  return(1 - ss_res / ss_tot)  # 计算 R^2
}

# 训练集 R^2
r2_train <- R2(birch.cross[idx.train, 1], pre.train)

# 测试集 R^2
r2_test <- R2(birch.cross[idx.test, 1], pre.test)

# 输出结果
cat("Training R^2:", r2_train, "\n")
cat("Testing R^2:", r2_test, "\n")

# 保存.RData文件
save.image(file = "test1.RData")