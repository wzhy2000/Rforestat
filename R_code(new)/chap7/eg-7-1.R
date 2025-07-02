flora <- read.csv("data-{flora}-7-1.csv", header=TRUE, sep=",")
attach(flora)

###### 1. 枫树数量随海拔的变化关系 ###############
model.qty <- glm(maple ~ elevation, family = poisson)
summary(model.qty)

elevationPoints <- seq(min(elevation), max(elevation), by = 0.01)
pre.link <- predict(model.qty, list(elevation = elevationPoints), type = "link")
head(pre.link)
pre.res <- predict(model.qty, list(elevation = elevationPoints), type = "response")
head(pre.res)
pre.terms <- predict(model.qty, list(elevation = elevationPoints), type = "terms")
head(pre.terms)


predictions <- predict(model.qty, list(elevation = elevationPoints), type = "response", se.fit = TRUE)

# 提取预测值和标准误差
predictions.res <- predictions$fit
predictions.upper <- predictions.res + 1.96 * predictions$se.fit # 上置信界限
predictions.lower <- predictions.res - 1.96 * predictions$se.fit # 下置信界限

pdf("图6.1a.pdf", height = 8, width = 8, family = "GB1")
par(mar = c(5, 5.5, 4, 2), mgp = c(3.5, 1, 0))
plot(elevation, maple, las = 1, pch = 16, col = "black", 
     cex = 2, cex.axis = 1.8, cex.lab = 1.8,
     xlab = "海拔高度(m)", ylab = "枫树数量")
# 绘制拟合曲线
lines(elevationPoints, predictions.res, lwd = 3, col = "blue")
# 绘制置信区间
lines(elevationPoints, predictions.upper, lwd = 2, col = "red", lty = 2) # 上界
lines(elevationPoints, predictions.lower, lwd = 2, col = "red", lty = 2) # 下界
dev.off()

res.response <- residuals(model.qty, type = "response")
head(res.response)
res.working <- residuals(model.qty, type = "working")
head(res.working)
res.pearson <- residuals(model.qty, type = "pearson")
head(res.pearson)
res.deviance <- residuals(model.qty, type = "deviance")
head(res.deviance)

fittedValues <- predict(model.qty, type = "response")
# 绘制残差图
pdf("图6.2a.pdf", height = 8, width = 8, family = "GB1")
par(mar = c(5, 5, 4, 2))
plot(fittedValues, res.response, xlab = "拟合值", 
     ylab = "残差", pch = 16, col = "black", cex = 1, 
     cex.lab = 1.8, cex.axis = 1.8)
abline(h = 0, col = "red")
dev.off()

pchisq(model.qty$null.deviance - model.qty$deviance, model.qty$df.null - model.qty$df.residual, lower.tail = FALSE)

anova(model.qty, test = "Chisq")

round(coef(model.qty), 4)
round(confint(model.qty), 4)
round(vcov(model.qty), 4)

###### 2. 枫树的相对丰度随海拔的变化关系 ###############
proportion <- maple / (otherTrees + maple)
sampleSize <- maple + otherTrees
model.prop <- glm(proportion ~ elevation, family = binomial, weights = sampleSize)
summary(model.prop)

predictions <- predict(model.prop, list(elevation = elevationPoints), type = "response", se.fit = TRUE)

# 提取预测值和标准误差
predictions.res <- predictions$fit
predictions.upper <- predictions.res + 1.96 * predictions$se.fit # 上置信界限
predictions.lower <- predictions.res - 1.96 * predictions$se.fit # 下置信界限

pdf("图6.1b.pdf", height = 8, width = 8, family = "GB1")
par(mar = c(5, 5.5, 4, 2), mgp = c(3.5, 1, 0))
plot(elevation, proportion, las = 1, pch = 16, col = "black", 
     cex = 2, cex.axis = 1.8, cex.lab = 1.8,
     xlab = "海拔高度(m)", ylab = "枫树比例")
# 绘制拟合曲线
lines(elevationPoints, predictions.res, lwd = 3, col = "blue")
# 绘制置信区间
lines(elevationPoints, predictions.upper, lwd = 2, col = "red", lty = 2) # 上界
lines(elevationPoints, predictions.lower, lwd = 2, col = "red", lty = 2) # 下界
dev.off()

res.response <- residuals(model.prop, type = "response")
predictions <- predict(model.prop, type = "response")
# 绘制残差图
pdf("图6.2b.pdf", height = 8, width = 8, family = "GB1")
par(mar = c(5, 5, 4, 2))
plot(predictions, res.response, xlab = "拟合值", 
     ylab = "残差", pch = 16, col = "black", cex = 1, 
     cex.lab = 1.8, cex.axis = 1.8)
abline(h = 0, col = "red")
dev.off()

anova(model.prop, test = "Chisq")

############# 3.glm.nb函数 ####################
dispersion <- sum(residuals(model.qty, type = "pearson")^2) / model.qty$df.residual
print(dispersion)
library(MASS)
model.glm.nb <- glm.nb(maple ~ elevation, data = flora, control = glm.control(maxit = 50))
summary(model.glm.nb)
