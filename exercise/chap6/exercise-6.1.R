library(forestat)
data("larch")

model1 <- lm(D ~ H, data = larch)
summary(model1)
