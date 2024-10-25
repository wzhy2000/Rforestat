flora <- read.table("flora.csv", header=TRUE, sep=",")
attach(flora)

model_1a <- glm(maple~elevation, family=poisson)
summary(model_1a)
summary(residuals(model_1a, type = "deviance"))
res.deviance <- residuals(model_1a, type = "deviance")

png("图6.1a.png", height = 800, width = 800)
plot(elevation, maple, las=1, pch=16, col="green", cex =2, cex.axis=1.8, cex.lab=1.8)
elevationPoints <- seq(min(elevation), max(elevation), by=0.01)
fittedValues <- predict(model_1a, list(elevation=elevationPoints), type="response")
lines(elevationPoints, fittedValues, lwd=3, col="blue")
dev.off()