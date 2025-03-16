# 数据加载
library(caret)
library(pROC)
library(pscl)

data <- read.csv("D:\\大连理工大学\\R语言书稿\\R_code(new)\\chap6\\data-exp-6-2.csv", fileEncoding = "GBK")


table(data$SH)
set.seed(123)


# （2） glm 模型拟合
model.poisson <- glm(SH ~ D0 + BA + CD + VH, family = poisson(link = "log"), data = data)
summary(model.poisson)

# 枯死概率
pre.prob.glm <- predict(model.poisson, type = "response")
head(pre.prob.glm)

# 将概率转换为二分类预测结果
pre.class.glm <- ifelse(pre.prob.glm > 0.5, 1, 0)
pre.class.glm <- as.factor(pre.class.glm)
pre.class.glm <- factor(pre.class.glm, levels = levels(as.factor(data$SH)))
# 生成混淆矩阵
confusionMatrix(pre.class.glm, as.factor(data$SH))

roc.curve.glm <- roc(data$SH, pre.prob.glm)
pdf("ROC.pdf", height = 8, width = 8)
par(mar = c(5, 6, 2, 2), mgp = c(4, 1, 0))
plot(roc.curve.glm, legacy.axes = TRUE,
     xlab = "False Positive Rate (1 - Specificity)",
     ylab = "True Positive Rate (Sensitivity)", 
     mar = c(6, 6, 2, 2), mgp = c(4, 1, 0), cex.lab = 2.2, cex.axis = 2.2)
dev.off()
auc(roc.curve.glm)  # 计算 AUC


# (3)  零膨胀模型
model.zif <- zeroinfl(SH ~ D0 + BA + CD + VH, dist = "poisson", data = data)
summary(model.zif)

pre.prob.zif <- predict(model.zif, type = "response")
head(pre.prob.zif)

# 将概率转换为二分类预测结果
pre.class.zif <- ifelse(pre.prob.zif > 0.5, 1, 0)
pre.class.zif <- as.factor(pre.class.zif)
pre.class.zif <- factor(pre.class.zif, levels = levels(as.factor(data$SH)))
# 生成混淆矩阵
confusionMatrix(pre.class.zif, as.factor(data$SH))

roc.curve.zif <- roc(data$SH, pre.prob.zif)
pdf("ROC2.pdf", height = 8, width = 8)
plot(roc.curve.zif, legacy.axes = TRUE, 
     xlab = "False Positive Rate (1 - Specificity)",
     ylab = "True Positive Rate (Sensitivity)", 
     mar = c(6, 6, 2, 2), mgp = c(4, 1, 0), cex.lab = 2.2, cex.axis = 2.2)
dev.off()
auc(roc.curve.zif)  # 计算 AUC

# (4) Hurdle模型
model.hurdle <- hurdle(SH ~ D0 + BA + CD + VH, dist = "poisson", data = data)
summary(model.hurdle)

pre.prob.hurdle <- predict(model.hurdle, type = "response")
head(pre.prob.hurdle)

# 将概率转换为二分类预测结果
pre.class.hurdle <- ifelse(pre.prob.hurdle > 0.5, 1, 0)
pre.class.hurdle <- as.factor(pre.class.hurdle)
pre.class.hurdle <- factor(pre.class.hurdle, levels = levels(as.factor(data$SH)))
# 生成混淆矩阵
confusionMatrix(pre.class.hurdle, as.factor(data$SH))

roc.curve.hurdle <- roc(data$SH, pre.prob.hurdle)
pdf("ROC3.pdf", height = 8, width = 8)
plot(roc.curve.hurdle, legacy.axes = TRUE, 
     xlab = "False Positive Rate (1 - Specificity)",
     ylab = "True Positive Rate (Sensitivity)", 
     mar = c(6, 6, 2, 2), mgp = c(4, 1, 0), cex.lab = 2.2, cex.axis = 2.2)
dev.off()
auc(roc.curve.hurdle)  # 计算 AUC


cat("GLM AUC:", auc(roc.curve.glm), ", ZIF AUC:", auc(roc.curve.zif), ", Hurdle AUC:", auc(roc.curve.hurdle), "\n")
