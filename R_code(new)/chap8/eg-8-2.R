library(forestat)
library(mgcv)
library(gamreg)
data("larch")
attach(larch)

model.gam <- gam(H ~ s(D) + s(CBH) + te(D, CBH), data = larch, method = "REML", select = TRUE)
summary(model.gam)

par(mar = c(5, 5, 4, 2), mgp = c(3.5, 1, 0))
plot(model.gam, pages = 3, cex.axis = 2, cex.lab = 2)

par(mar = c(5, 5, 4, 2), mgp = c(3.5, 1, 0))
gam.check(model.gam, pch = 19, cex = .5, cex.axis = 2, cex.lab = 2)


Y <- as.matrix(H)
X <- as.matrix(larch[, c("D", "CBH")])
res <- cv.gam(X = X, Y = Y, nlambda = 10, init.mode = "sLTS", fold = 5, lmin = 0.01, lmax = 1)
res$lambda
res$Rocv
