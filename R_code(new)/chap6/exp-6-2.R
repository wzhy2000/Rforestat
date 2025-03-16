# 数据加载
library(forestat)
library(caret)
library(pROC)
library(pscl)
data("larch")

table(larch$LIFE)
set.seed(123)
# 移除 unknown 数据
larch <- larch[larch$LIFE != "unknown", ]
# 重新定义枯死状态的二分类变量（0 表示存活，1 表示枯死）
is.dead <- ifelse(larch$LIFE %in% c("broken_head", "broken_tip"), 1, 0)
# 检查新变量分布
table(is.dead)


# （2） glm 模型拟合
model.poisson <- glm(is.dead ~ H + D + CBH + CW, family = poisson(link = "log"), data = larch)
summary(model.poisson)

# 枯死概率
pre.prob.glm <- predict(model.poisson, type = "response")
head(pre.prob.glm)

# 将概率转换为二分类预测结果
pre.class.glm <- ifelse(pre.prob.glm > 0.5, 1, 0)
pre.class.glm <- as.factor(pre.class.glm)
pre.class.glm <- factor(pre.class.glm, levels = levels(as.factor(is.dead)))
# 生成混淆矩阵
confusionMatrix(pre.class.glm, as.factor(is.dead))

roc.curve.glm <- roc(is.dead, pre.prob.glm)
pdf("ROC.pdf", height = 8, width = 8)
par(mar = c(5, 6, 2, 2), mgp = c(4, 1, 0))
plot(roc.curve.glm, legacy.axes = TRUE,
     xlab = "False Positive Rate (1 - Specificity)",
     ylab = "True Positive Rate (Sensitivity)", 
     mar = c(6, 6, 2, 2), mgp = c(4, 1, 0), cex.lab = 2.2, cex.axis = 2.2)
dev.off()
auc(roc.curve.glm)  # 计算 AUC


# (3)  零膨胀模型
model.zif <- zeroinfl(is.dead ~ H + D + CBH + CW, dist = "poisson", data = larch)
summary(model.zif)

pre.prob.zif <- predict(model.zif, type = "response")
head(pre.prob.zif)

# 将概率转换为二分类预测结果
pre.class.zif <- ifelse(pre.prob.zif > 0.5, 1, 0)
pre.class.zif <- as.factor(pre.class.zif)
pre.class.zif <- factor(pre.class.zif, levels = levels(as.factor(is.dead)))
# 生成混淆矩阵
confusionMatrix(pre.class.zif, as.factor(is.dead))

roc.curve.zif <- roc(is.dead, pre.prob.zif)
pdf("ROC2.pdf", height = 8, width = 8)
plot(roc.curve.zif, legacy.axes = TRUE, 
     xlab = "False Positive Rate (1 - Specificity)",
     ylab = "True Positive Rate (Sensitivity)", 
     mar = c(6, 6, 2, 2), mgp = c(4, 1, 0), cex.lab = 2.2, cex.axis = 2.2)
dev.off()
auc(roc.curve.zif)  # 计算 AUC

# (4) Hurdle模型
model.hurdle <- hurdle(is.dead ~ H + D + CBH + CW, dist = "poisson", data = larch)
summary(model.hurdle)

pre.prob.hurdle <- predict(model.hurdle, type = "response")
head(pre.prob.hurdle)

# 将概率转换为二分类预测结果
pre.class.hurdle <- ifelse(pre.prob.hurdle > 0.5, 1, 0)
pre.class.hurdle <- as.factor(pre.class.hurdle)
pre.class.hurdle <- factor(pre.class.hurdle, levels = levels(as.factor(is.dead)))
# 生成混淆矩阵
confusionMatrix(pre.class.hurdle, as.factor(is.dead))

roc.curve.hurdle <- roc(is.dead, pre.prob.hurdle)
pdf("ROC3.pdf", height = 8, width = 8)
plot(roc.curve.hurdle, legacy.axes = TRUE, 
     xlab = "False Positive Rate (1 - Specificity)",
     ylab = "True Positive Rate (Sensitivity)", 
     mar = c(6, 6, 2, 2), mgp = c(4, 1, 0), cex.lab = 2.2, cex.axis = 2.2)
dev.off()
auc(roc.curve.hurdle)  # 计算 AUC


cat("GLM AUC:", auc(roc.curve.glm), ", ZIF AUC:", auc(roc.curve.zif), ", Hurdle AUC:", auc(roc.curve.hurdle), "\n")