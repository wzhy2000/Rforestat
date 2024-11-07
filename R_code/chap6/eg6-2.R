flora <- read.csv("data-{flora}-6-1.csv", header=TRUE, sep=",")
attach(flora)
proportion <- maple/(otherTrees+maple)
sampleSize <- maple + otherTrees
model_2a <- glm(proportion~elevation, family=binomial, weights=sampleSize)

pdf("图6.1b.pdf", height = 8, width = 8)
plot(elevation, proportion, las=1, pch=16, col="green", cex = 2, cex.axis=1.8, cex.lab=1.8)
elevationPoints <- seq(min(elevation), max(elevation), by=0.01)
fittedValues <- predict(model_2a, list(elevation=elevationPoints), type="response")
lines(elevationPoints, fittedValues, lwd=3, col="blue")
dev.off()