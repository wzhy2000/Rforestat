library(readxl)      # 用于读取 Excel 文件
library(ggplot2)     # 用于可视化分析
library(forestat)

set.seed(123)        # 保证抽样结果可复现
df <- read_excel("Raw_data.xlsx", sheet = 2)  #导入原始数据
idx <- sample(1:nrow(df), 0.7 * nrow(df))
train <- df[idx, ]#将70%的数据划分为建模所需的训练集
test <- df[-idx, ]#剩余（30%）数据作为测试集以便独立验证模型的泛化能力

nls.model <- nls(`Crown(m2)` ~ a * `H(m)`^b, data = train, start = list(a = 1, b = 1))#最小二乘法估计参数 a 与 b，并且初始值均设定为1。
summary(nls.model)
#为了比较建模方式的差异，使用相同训练集建立线性回归模型
lm.model <- lm(`Crown(m2)` ~ `H(m)`, data = train)
summary(lm.model)   


nls.pred.test <- predict(nls.model, newdata = test)
nls.obs.test <- test$`Crown(m2)`
FittingEvaluationIndex(nls.pred.test, nls.obs.test)


lm.pred.test <- predict(lm.model, newdata = test)
lm.obs.test <- test$`Crown(m2)`
FittingEvaluationIndex(lm.pred.test, lm.obs.test)

# 残差图
nls.res.test <- test$`Crown(m2)` - nls.pred.test
pdf("非线性回归模型测试集上残差图.pdf", width = 8, height = 8, family = "GB1")
par(mar = c(5, 5, 4, 2), mgp = c(3, 1, 0))
plot(nls.pred.test, nls.res.test, xlab = "冠层面积(m2)", 
     ylab = "残差(m2)", pch = 16, col = "black", cex = 1, 
     cex.lab = 2.5, cex.axis = 2.5)
abline(h = 0, col = "red")
dev.off()

# 残差图
lm.res.test <- test$`Crown(m2)` - lm.pred.test
pdf("线性回归模型测试集上残差图.pdf", width = 8, height = 8, family = "GB1")
par(mar = c(5, 5, 4, 2), mgp = c(3, 1, 0))
plot(lm.pred.test, lm.res.test, xlab = "冠层面积(m2)", 
     ylab = "残差(m2)", pch = 16, col = "black", cex = 1, 
     cex.lab = 2.5, cex.axis = 2.5)
abline(h = 0, col = "red")
dev.off()


# 拟合曲线
pdf("测试集上拟合图.pdf", width = 8, height = 8, family = "GB1")
par(mar = c(5, 5, 4, 2), mgp = c(3, 1, 0))
plot(test$`H(m)`, test$`Crown(m2)`, pch = 16, col = "black",
     xlab = "树高(m)", ylab = " 冠层面积(m2)", cex.lab = 2.5, cex.axis = 2.5)
lines(sort(test$`H(m)`),
      predict(lm.model, newdata = test[order(test$`H(m)`), ]), col = "blue", lwd = 2)
lines(sort(test$`H(m)`),
      predict(nls.model, newdata = test[order(test$`H(m)`), ]), col = "red", lwd = 2)
legend("topleft", legend = c("线性", "非线性"), col = c("blue", "red"), lwd = 2, cex = 2.5)

dev.off()
