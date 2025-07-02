library("splines")
library("locfit")
library(forestat)
data("larch")
data1 <- larch
attach(data1)
plot(data1$H,data1$CW)

xlims=range(D)
x.grid=seq(from=xlims[1],to=xlims[2])

# 回归样条
plot_model<-function(x,y,preds,x.grid,xlims){
  par(mar = c(4, 5.2, 2, 2), mgp = c(3, 1, 0))
  plot(x,y,xlim=xlims,col="darkgray",xlab="胸径(cm)",ylab="冠幅(m)", cex.lab = 2, cex.axis = 2)
  lines(x.grid,preds$fit,lwd=2,col="blue")
  lines(x.grid,preds$fit+2*preds$se,lty = "dashed")
  lines(x.grid,preds$fit-2*preds$se,lty = "dashed")
}

pdf("图8.2a回归样条(节点数为1).pdf", width = 8, height = 4, family = "GB1")
fit2 = lm(CW ~ bs(D, df = 4), data = data1)
summary(fit2)
preds2 = predict(fit2, newdata = list(D = x.grid), se = TRUE)
par(mar = c(4, 5.2, 2, 2), mgp = c(3, 1, 0))
plot_model(D,CW,preds2,x.grid,xlims)
bs_x <- bs(D, df = 4)
actual_knots <- attr(bs_x, "knots")
y_knots <- predict(fit2, newdata = data.frame(D = actual_knots))
points(actual_knots, y_knots, col="red", pch=19,cex=.5)
for (i in 1:length(actual_knots)) {
  abline(v = actual_knots[i], lty = 2, col = "red")
}
dev.off()

pdf("图8.2c回归样条（节点数为2）.pdf", width = 8, height = 4, family = "GB1")
fit2=lm(CW~bs(D,df=5), data=data1)
summary(fit2)
preds2=predict(fit2,newdata=list(D=x.grid),se=TRUE)
par(mar = c(4, 5.2, 2, 2), mgp = c(3, 1, 0))
plot_model(D,CW,preds2,x.grid,xlims)
bs_x <- bs(D, df = 5)
actual_knots <- attr(bs_x, "knots")
y_knots <- predict(fit2, newdata = data.frame(D = actual_knots))
points(actual_knots, y_knots, col="red", pch=19,cex=.5)
for (i in 1:length(actual_knots)) {
  abline(v = actual_knots[i], lty = 2, col = "red")
}
dev.off()

pdf("图8.2e回归样条（节点数为3）.pdf", width = 8, height = 4, family = "GB1")
fit2=lm(CW~bs(D,df=6), data=data1)
summary(fit2)
preds2=predict(fit2,newdata=list(D=x.grid),se=TRUE)
par(mar = c(4, 5.2, 2, 2), mgp = c(3, 1, 0))
plot_model(D,CW,preds2,x.grid,xlims)
bs_x <- bs(D, df = 6)
actual_knots <- attr(bs_x, "knots")
y_knots <- predict(fit2, newdata = data.frame(D = actual_knots))
points(actual_knots, y_knots, col="red", pch=19,cex=.5)
for (i in 1:length(actual_knots)) {
  abline(v = actual_knots[i], lty = 2, col = "red")
}
dev.off()

# 自然样条
pdf("图8.2b自然样条（节点数为1）.pdf", width = 8, height = 4, family = "GB1")
fit3=lm(CW~ns(D,df=2), data=data1)
summary(fit3)
preds3=predict(fit3,newdata=list(D=x.grid),se=TRUE)
par(mar = c(4, 5.2, 2, 2), mgp = c(3, 1, 0))
plot_model(D,CW,preds3,x.grid,xlims)
ns_x <- ns(D, df = 2)
actual_knots <- attr(ns_x, "knots")
y_knots <- predict(fit3, newdata = data.frame(D = actual_knots))
points(actual_knots, y_knots, col="red", pch=19,cex=.5)
for (i in 1:length(actual_knots)) {
  abline(v = actual_knots[i], lty = 2, col = "red")
}
dev.off()

pdf("图8.2d自然样条（节点数为2）.pdf", width = 8, height = 4, family = "GB1")
fit3=lm(CW~ns(D,df=3), data=data1)
summary(fit3)
preds3=predict(fit3,newdata=list(D=x.grid),se=TRUE)
par(mar = c(4, 5.2, 2, 2), mgp = c(3, 1, 0))
plot_model(D,CW,preds3,x.grid,xlims)
ns_x <- ns(D, df = 3)
actual_knots <- attr(ns_x, "knots")
y_knots <- predict(fit3, newdata = data.frame(D = actual_knots))
points(actual_knots, y_knots, col="red", pch=19,cex=.5)
for (i in 1:length(actual_knots)) {
  abline(v = actual_knots[i], lty = 2, col = "red")
}
dev.off()

pdf("图8.2f自然样条（节点数为3）.pdf", width = 8, height = 4, family = "GB1")
fit3=lm(CW~ns(D,df=4), data=data1)
summary(fit3)
preds3=predict(fit3,newdata=list(D=x.grid),se=TRUE)
par(mar = c(4, 5.2, 2, 2), mgp = c(3, 1, 0))
plot_model(D,CW,preds3,x.grid,xlims)
ns_x <- ns(D, df = 4)
actual_knots <- attr(ns_x, "knots")
y_knots <- predict(fit3, newdata = data.frame(D = actual_knots))
points(actual_knots, y_knots, col="red", pch=19,cex=.5)
for (i in 1:length(actual_knots)) {
  abline(v = actual_knots[i], lty = 2, col = "red")
}
dev.off()

# 光滑样条
plot(D,CW,xlim=xlims,cex=1, col="darkgrey",xlab="D",ylab="CW")
fit4.1=smooth.spline(D,CW,df=5)
fit4.2=smooth.spline(D,CW,cv=TRUE)
fit4.2
fit4.2$df
pdf("图8.2g光滑样条（df=5）.pdf", width = 8, height = 4, family = "GB1")
par(mar = c(4, 5.2, 2, 2), mgp = c(3, 1, 0))
plot(D,CW,xlim=xlims,cex=1, col="darkgrey",xlab="胸径(cm)",ylab="冠幅(m)", cex.lab = 2, cex.axis = 2)
lines(fit4.1,lwd=2,col="red")
dev.off()
pdf("图8.2h光滑样条（df=9）.pdf", width = 8, height = 4, family = "GB1")
par(mar = c(4, 5.2, 2, 2), mgp = c(3, 1, 0))
plot(D,CW,xlim=xlims,cex=1, col="darkgrey",xlab="胸径(cm)",ylab="冠幅(m)", cex.lab = 2, cex.axis = 2)
lines(fit4.2,lwd=2,col="blue")
dev.off()
legend("topright",legend=c("6 DF","9 DF"),col=c("red","blue"),lty=1,lwd=2,cex=.8)


# 局部回归
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
  plot(x, y, cex = 0.5, col = "darkgray", xlab = "胸径(cm)", ylab = "冠幅(m)", cex.lab = 2, cex.axis = 2)
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
