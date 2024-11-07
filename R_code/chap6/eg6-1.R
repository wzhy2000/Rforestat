flora <- read.csv("data-{flora}-6-1.csv", header=TRUE, sep=",")
attach(flora)

model_1a <- glm(maple~elevation, family=poisson)
summary(model_1a)
summary(residuals(model_1a, type = "deviance"))
res.deviance <- residuals(model_1a, type = "deviance")

pdf("图6.1a.pdf", height = 8, width = 8)
plot(elevation, maple, las=1, pch=16, col="green", cex =2, cex.axis=1.8, cex.lab=1.8)
elevationPoints <- seq(min(elevation), max(elevation), by=0.01)
fittedValues <- predict(model_1a, list(elevation=elevationPoints), type="response")
lines(elevationPoints, fittedValues, lwd=3, col="blue")
dev.off()
