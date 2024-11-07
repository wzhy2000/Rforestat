library(mgcv)
set.seed(6)
n <- 200
sig <- 2
dat <- gamSim(1, n = n, scale = sig)
b <- gam(y ~ s(x0) + s(I(x1^2)) + s(x2) + offset(x3), data = dat)

newd <- data.frame(x0 = (0:30) / 30, x1 = (0:30) / 30, x2 = (0:30) / 30, x3 = (0:30) / 30)  
pred <- predict.gam(b, newd)
pred0 <- predict(b, newd, exclude = "s(x0)")
head(pred)
head(pred0)
pdf("图9.1.pdf", width = 8, height = 4, family = "GB1")
plot(newd$x0, pred, type = "l", col = "black", lwd = 2,
     main = "Prediction Results", xlab = "x0", ylab = "Predicted Values",   
     col.axis = "black", col.lab = "black", col.main = "black")
dev.off()