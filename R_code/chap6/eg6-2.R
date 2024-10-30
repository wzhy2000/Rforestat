flora <- read.table("data6-1.csv", header=TRUE, sep=",")
attach(flora)
proportion <- maple/(otherTrees+maple)
sampleSize <- maple + otherTrees
model_2a <- glm(proportion~elevation, family=binomial, weights=sampleSize)

png("图6.1b.png", height = 800, width = 800)
plot(elevation, proportion, las=1, pch=16, col="green", cex = 2, cex.axis=1.8, cex.lab=1.8)
elevationPoints <- seq(min(elevation), max(elevation), by=0.01)
fittedValues <- predict(model_2a, list(elevation=elevationPoints), type="response")
lines(elevationPoints, fittedValues, lwd=3, col="blue")
dev.off()