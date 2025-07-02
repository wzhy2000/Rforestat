library("splines")
library("locfit")
library(forestat)
data("larch")
data1 <- larch
attach(data1)
# 局部回归
xlims <- range(larch$D)
x.grid=seq(from=xlims[1],to=xlims[2])
plot(D,CW,xlim=xlims,cex=1, col="darkgrey",xlab="D",ylab="CW")
fit5.1=loess(CW~D,span=.1,data=data1)
fit5.2=loess(CW~D,span=.5,data=data1)
summary(fit5.2)
lines(x.grid,predict(fit5.1,data.frame(D=x.grid)),lwd=2,col="red")
lines(x.grid,predict(fit5.2,data.frame(D=x.grid)),lwd=2,col="blue")
legend("topright",legend=c("Span=0.2","Span=0.5"),col=c("red","blue"),lty=1,lwd=2,cex=.8)

xlims <- range(larch$D)
x.seq <- seq(from = xlims[1], to = xlims[2])
model.lo1 <- loess(CW ~ D, span = 0.1, data = larch)
model.lo5 <- loess(CW ~ D, span = 0.5, data = larch)

plot.model <- function(x, y, x.seq, y.preds){
  par(mar = c(4, 5.2, 2, 2), mgp = c(3, 1, 0))
  plot(x, y, cex = 0.5, col = "darkgray", xlab = "胸径/cm", ylab = "冠幅/m", cex.lab = 2, cex.axis = 2)
  lines(x.seq, y.preds$fit, lwd = 2, col = "blue")
  lines(x.seq, y.preds$fit + 2 * y.preds$se.fit, lty = "dashed")
  lines(x.seq, y.preds$fit - 2 * y.preds$se.fit, lty = "dashed")
}

pdf("局部回归0.1.pdf", width = 8, height = 6, family = "GB1")
y.preds <- predict(model.lo1, data.frame(D = x.seq), se = TRUE)
plot.model(larch$D, larch$CW, x.seq, y.preds)
dev.off()

pdf("局部回归0.5.pdf", width = 8, height = 6, family = "GB1")
y.preds <- predict(model.lo5, data.frame(D = x.seq), se = TRUE)
plot.model(larch$D, larch$CW, x.seq, y.preds)
dev.off()