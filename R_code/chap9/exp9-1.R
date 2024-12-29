library("splines")
library("locfit")
library("forestat")

FittingEvaluationIndex <- function(EstiH, ObsH) {
  Index<-array(dim=6)
  e<-ObsH-EstiH
  e1<-ObsH-mean(ObsH)
  pe<-mean(e)
  var2<-var(e)
  var<-sqrt(var(e))
  RMSE<-sqrt(pe^2+var2)
  R2<-1-sum(e^2)/sum((e1)^2)
  TRE<-100*sum(e^2)/sum((EstiH)^2)
  Index[1]<-pe
  Index[2]<-RMSE
  Index[3]<-R2
  Index[4]<-var2
  Index[5]<-TRE
  Index[6]<-var
  dimnames(Index)<-list(c("pe","RMSE","R2","Var","TRE","sd"))
  return(Index)
}


# 1.数据加载
data(larch)
data1 <- larch
attach(data1)
names(data1)
dim(data1)

# 2.多项式回归模型
fit.p4 = lm(CW ~ poly(D, 4, raw=T), data = data1)
summary(fit.p4)
cat(AIC(fit.p4), BIC(fit.p4))
FittingEvaluationIndex(predict(fit.p4, newdata = data1), data1$CW)

fit.p4a = lm(CW ~ D + I(D^2) + I(D^3) + I(D^4), data = data1)
summary(fit.p4a)
fit.p4b = lm(CW ~ cbind(D, D^2, D^3, D^4), data = data1)
summary(fit.p4b)

xlims = range(data1$D)
x.grid = seq(from = xlims[1], to = xlims[2])
preds = predict(fit.p4, newdata = list(D = x.grid), se = TRUE)
se.bands = cbind(preds$fit + 2 * preds$se.fit, preds$fit - 2 * preds$se.fit)

plot(D, CW, xlim = xlims, cex = .5, col = "darkgrey")
lines(x.grid, preds$fit, lwd = 2, col = "blue")
matlines(x.grid, se.bands, lwd = 1, co1 = "blue", lty = 3)

fit.p1 = lm(CW ~ D, data = data1)
fit.p2 = lm(CW ~ poly(D, 2, raw=T), data = data1)
fit.p3 = lm(CW ~ poly(D, 3, raw = T), data = data1)
fit.p4 = lm(CW ~ poly(D, 4, raw = T), data = data1)
fit.p5 = lm(CW ~ poly(D, 5, raw = T), data = data1)
fit.p6 = lm(CW ~ poly(D, 6, raw = T), data = data1)
fit.p7 = lm(CW ~ poly(D, 7, raw = T), data = data1)
fit.p8 = lm(CW ~ poly(D, 8, raw = T), data = data1)
fit.p9 = lm(CW ~ poly(D, 9, raw = T), data = data1)
anova(fit.p1, fit.p2, fit.p3, fit.p4, fit.p5, fit.p6, fit.p7, fit.p8, fit.p9)

# 3.回归样条
fit.bs = lm(CW ~ bs(D, df = 6), data = data1)
summary(fit.bs)
preds2 = predict(fit.bs, newdata = list(D = x.grid), se = TRUE)

cat(AIC(fit.bs), BIC(fit.bs))
FittingEvaluationIndex(predict(fit.bs, newdata = data1), data1$CW)

plot_model <- function(x, y, preds, x.grid, xlims){
  plot(x, y, xlim = xlims, col = "darkgray", xlab = "D", ylab = "CW")
  lines(x.grid, preds$fit, lwd = 2, col = "blue")
  lines(x.grid, preds$fit + 2 * preds$se, lty = "dashed")
  lines(x.grid, preds$fit - 2 * preds$se, lty = "dashed")
}

plot_model(D, CW, preds2, x.grid, xlims)
bs_x <- bs(D, df = 6)
actual_knots <- attr(bs_x, "knots")
y_knots <- predict(fit.bs, newdata = data.frame(D = actual_knots))
points(actual_knots, y_knots, col="red", pch=19,cex=.5)
for (i in 1:length(actual_knots)) {
  abline(v = actual_knots[i], lty = 2, col = "red")
}

# 4.自然样条
fit.ns = lm(CW ~ ns(D, df = 4), data = data1)
summary(fit.ns)
preds3=predict(fit.ns, newdata = list(D = x.grid), se = TRUE)

cat(AIC(fit.ns), BIC(fit.ns))
FittingEvaluationIndex(predict(fit.ns, newdata = data1), data1$CW)

# 5.光滑样条
fit.s1 = smooth.spline(data1$D, data1$CW, df = 5)
fit.s2 = smooth.spline(data1$D, data1$CW, cv = TRUE)
fit.s2$df
fit.s1
summary(fit.s1)
pre <- rep(NA, length(data1$D))
t = predict(fit.s2, newdata = data1$D)
for (i in seq_along(data1$D)) {
  p <- match(data1$D[i], t$x)
  if (!is.na(p)) {
    pre[i] <- t$y[p]
  } else {
    pre[i] <- NA
  }
}

FittingEvaluationIndex(pre, data1$CW)
# install.packages("santaR")
library(santaR)
help(santaR)
AIC_smooth_spline(fit.s1)

plot(D,CW,xlim=xlims,cex=1, col="darkgrey",xlab="D",ylab="CW")
lines(fit.s1,lwd=2,col="red")
plot(D,CW,xlim=xlims,cex=1, col="darkgrey",xlab="D",ylab="CW")
lines(fit.s2,lwd=2,col="blue")

# 6.局部回归
fit.lo1 = loess(CW ~ D, span = .1, data = data1)
fit.lo2 = loess(CW ~ D, span = .5, data = data1)
summary(fit.lo1)
FittingEvaluationIndex(predict(fit.lo1, newdata = data1), data1$CW)
FittingEvaluationIndex(predict(fit.lo2, newdata = data1), data1$CW)

plot(D, CW, xlim = xlims, cex = 1, col = "darkgrey", xlab = "D", ylab = "CW")
lines(x.grid, predict(fit.lo1, data.frame(D = x.grid)), lwd = 2, col = "red")
plot(D,CW, xlim = xlims, cex = 1, col = "darkgrey", xlab = "D", ylab = "CW")
lines(x.grid, predict(fit.lo2, data.frame(D = x.grid)), lwd = 2, col = "blue")

