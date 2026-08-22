flora <- read.csv("../../data/example-7.1.csv", header = TRUE, sep = ",")
# [修订 CH7-095] 删除可能造成变量遮蔽的attach()，统一通过data参数传入数据

###### 1. 枫树数量随海拔的变化关系 ###############
model.qty <- glm(maple ~ elevation, family = poisson, data = flora)
summary(model.qty)

elevationPoints <- seq(min(flora$elevation), max(flora$elevation), by = 0.01)
pre.link <- predict(model.qty, list(elevation = elevationPoints), type = "link")
head(pre.link)
pre.res <- predict(model.qty, list(elevation = elevationPoints), type = "response")
head(pre.res)
pre.terms <- predict(model.qty, list(elevation = elevationPoints), type = "terms")
head(pre.terms, n = 3)


# [修订 CH7-110] 先在连接函数尺度构造区间，再用逆连接函数转换到响应尺度
pred.link <- predict(model.qty, newdata = data.frame(elevation = elevationPoints),
                     type = "link", se.fit = TRUE)
predictions.res <- model.qty$family$linkinv(pred.link$fit)
predictions.lower <- model.qty$family$linkinv(pred.link$fit - 1.96 * pred.link$se.fit)
predictions.upper <- model.qty$family$linkinv(pred.link$fit + 1.96 * pred.link$se.fit)

pdf("图6.1a.pdf", height = 8, width = 8, family = "GB1")
par(mar = c(5, 5.5, 4, 2), mgp = c(3.5, 1, 0))
plot(flora$elevation, flora$maple, las = 1, pch = 16, col = "black",
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
flora$proportion <- flora$maple / (flora$otherTrees + flora$maple)
flora$sampleSize <- flora$maple + flora$otherTrees
model.prop <- glm(proportion ~ elevation, family = binomial,
                  weights = sampleSize, data = flora)
summary(model.prop)

# [修订 CH7-110] 比例模型的置信区间同样在连接尺度构造后再转换
pred.link.prop <- predict(model.prop, newdata = data.frame(elevation = elevationPoints),
                          type = "link", se.fit = TRUE)
predictions.res <- model.prop$family$linkinv(pred.link.prop$fit)
predictions.lower <- model.prop$family$linkinv(pred.link.prop$fit - 1.96 * pred.link.prop$se.fit)
predictions.upper <- model.prop$family$linkinv(pred.link.prop$fit + 1.96 * pred.link.prop$se.fit)

pdf("图6.1b.pdf", height = 8, width = 8, family = "GB1")
par(mar = c(5, 5.5, 4, 2), mgp = c(3.5, 1, 0))
plot(flora$elevation, flora$proportion, las = 1, pch = 16, col = "black",
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
