# (1) 数据加载
library("forestat")
data(birch)
dim(birch)
names(birch)
summary(birch[, c("D", "H", "CBH", "CLR", "CW")])
birch$H <- round(birch$H * 100)
set.seed(123)

# 划分索引，70% 的数据用于训练
train.index <- sample(1:nrow(birch), size = 0.7 * nrow(birch))

# 创建训练集和测试集
train.data <- birch[train.index, ]
test.data <- birch[-train.index, ]

attach(birch)


# (2) 模型拟合
model.null <- glm(H ~ 1, family = poisson(link = "log"), data = train.data)

model.log <- glm(H ~ D + CBH + CW, family = poisson(link = "log"), data = train.data)
summary(model.log)
p.value.log <- pchisq(model.log$null.deviance - model.log$deviance, model.log$df.null - model.log$df.residual, lower.tail = FALSE)
p.value.log
pre.log <- predict(model.log, type = "response")


model.idn <- glm(H ~ D + CBH + CW, family = poisson(link = "identity"), data = train.data)
summary(model.idn)
p.value.idn <- pchisq(model.idn$null.deviance - model.idn$deviance, model.idn$df.null - model.idn$df.residual, lower.tail = FALSE)
p.value.idn
pre.idn <- predict(model.idn, type = "response")


model.sqrt <- glm(H ~ D + CBH + CW, family = poisson(link = "sqrt"), data = train.data)
summary(model.sqrt)
p.value.sqrt <- pchisq(model.sqrt$null.deviance - model.sqrt$deviance, model.sqrt$df.null - model.sqrt$df.residual, lower.tail = FALSE)
p.value.sqrt
pre.sqrt <- predict(model.sqrt, type = "response")


# (3) 性能评估
FittingEvaluationIndex(pre.log, train.data$H)
FittingEvaluationIndex(pre.idn, train.data$H)
FittingEvaluationIndex(pre.sqrt, train.data$H)
anova(model.log, model.idn, test = "Chisq")

# (4) 模型选择  
cat("AIC: ", "Log:", AIC(model.log), "Identity:", AIC(model.idn), "Sqrt:", AIC(model.sqrt), "\n")

cat("BIC: ", "Log:", BIC(model.log), "Identity:", BIC(model.idn), "Sqrt:", BIC(model.sqrt), "\n")


# (5) 模型测试

pre.test <- predict(model.idn, newdata = test.data, type = "response")
FittingEvaluationIndex(pre.test, test.data$H)


# (6) 可视化
# 拟合图
pdf("fit.plot.pdf", width = 8, height = 8, family = "GB1")
par(mar = c(5, 5, 4, 2), mgp = c(3.5, 1, 0))
plot(pre.test, test.data$H, xlab = "拟合树高(cm)", ylab = "树高(cm)", las = 1, 
     pch = 16, col = "black", cex = 1, 
    cex.lab = 2.5, cex.axis = 2.5)
dev.off()

# 训练集残差图
pre.train <- predict(model.idn, type = "response")
res.train <- residuals(model.idn, type = "response")
pdf("Residuals.train.pdf", width = 8, height = 8, family = "GB1")
par(mar = c(5, 5, 4, 2), mgp = c(3.5, 1, 0))
plot(pre.train, res.train, xlab = "拟合树高(cm)", 
     ylab = "残差(cm)", pch = 16, col = "black", cex = 1, 
     cex.lab = 2.5, cex.axis = 2.5)
abline(h = 0, col = "red")
dev.off()


# 测试集残差图
res.test <- test.data$H - pre.test
pdf("Residuals.test.pdf", width = 8, height = 8, family = "GB1")
par(mar = c(5, 5, 4, 2), mgp = c(3.5, 1, 0))
plot(pre.test, res.test, xlab = "拟合树高(cm)", 
     ylab = "残差(cm)", pch = 16, col = "black", cex = 1, 
     cex.lab = 2.5, cex.axis = 2.5)
abline(h = 0, col = "red")
dev.off()
