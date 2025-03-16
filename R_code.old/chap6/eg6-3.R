floraPA <- read.csv("data-{floraPA}-6-3.csv", header=TRUE, sep=",")
attach(floraPA)

################（1） 零膨胀模型 ####################
model.zif <- zeroinfl(pine ~ temperature, dist = "poisson", data = floraPA)
summary(model.zif)

pre.response <- predict(model.zif, type = "response")
head(pre.response)
pre.prob <- predict(model.zif, type = "prob")
head(pre.prob)
pre.count <- predict(model.zif, type = "count")
head(pre.count)
pre.zero <- predict(model.zif, type = "zero")
head(pre.zero)


pR2(model.zif)

res.response <- residuals(model.zif, type = "response")
head(res.response)
res.pearson <- residuals(model.zif, type = "pearson")
head(res.pearson)

pre.prob <- predprob(model.zif)
head(pre.prob)

vcov.zif <- vcov(model.zif)
print(vcov.zif)

temperaturePoints <- seq(min(temperature), max(temperature), by = 0.01)
predictions <- predict(model.zif, list(temperature = temperaturePoints), type = "response")


# pdf("图6.3a.pdf", height = 8, width = 8)
plot(temperature, pine, las = 1, pch = 16, col = "black", 
     cex = 2, cex.axis = 1.8, cex.lab = 1.8,
     xlab = "temperature", ylab = "pine")
# 绘制拟合曲线
lines(temperaturePoints, predictions, lwd = 3, col = "blue")
# dev.off()

par(mar = c(5, 5, 4, 2))
plot(pre.response, res.response, xlab = "拟合值", 
     ylab = "残差", pch = 16, col = "black", cex = 1, 
     cex.lab = 1.8, cex.axis = 1.8)
abline(h = 0, col = "red")

#################### （2） Hurdle模型 #################
model.hurdle <- hurdle(pine ~ temperature, dist = "poisson", data = floraPA)
summary(model.hurdle)

pre.response <- predict(model.hurdle, type = "response")
head(pre.response)
pre.prob <- predict(model.hurdle, type = "prob")
head(pre.prob)
pre.count <- predict(model.hurdle, type = "count")
head(pre.count)
pre.zero <- predict(model.hurdle, type = "zero")
head(pre.zero)

pR2(model.hurdle)


predictions <- predict(model.hurdle, list(temperature = temperaturePoints), type = "response")
plot(temperature, pine, las = 1, pch = 16, col = "black", 
     cex = 2, cex.axis = 1.8, cex.lab = 1.8,
     xlab = "temperature", ylab = "pine")
# 绘制拟合曲线
lines(temperaturePoints, predictions, lwd = 3, col = "blue")

res.response <- residuals(model.hurdle, type = "response")
pre.response <- predict(model.hurdle, type = "response")

par(mar = c(5, 5, 4, 2))
plot(pre.response, res.response, xlab = "拟合值", 
     ylab = "残差", pch = 16, col = "black", cex = 1, 
     cex.lab = 1.8, cex.axis = 1.8)
abline(h = 0, col = "red")
