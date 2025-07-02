library(forestat)
data(larch)
attach(larch)
dim(larch)


pr = princomp(~D + H + CW + CLR + CBH, data = larch, subset = 1:30)

summary(pr, loadings = TRUE)

head(predict(pr), n=4)

screeplot(pr, type = "lines")
biplot(pr, scale = 0.5)
