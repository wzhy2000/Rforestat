library(forestat)
library(splines)
data(larch)
attach(larch)

########### 1.多项式样条 #################
# 三次多项式回归模型
model.p3 <- lm(CW ~ poly(D, 3), data = larch)
summary(model.p3)

# 绘图
pdf("多项式2阶.pdf", height = 6, width = 8, family = "GB1")
fit1=lm(CW~poly(D,2), data=larch)
summary(fit1)
# ,raw=T
xlims=range(D)
x.grid=seq(from=xlims[1],to=xlims[2])
preds=predict(fit1,newdata=list(D=x.grid),se=TRUE)
se.bands=cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit)
par(mar = c(4, 5.2, 2, 2), mgp = c(3, 1, 0))
plot(D,CW,xlim=xlims, col="darkgrey",xlab="胸径(cm)",ylab="冠幅(m)", cex.lab = 2.5, cex.axis = 2.5)
lines(x.grid,preds$fit,lwd=3,col="blue")
matlines(x.grid,se.bands,lwd=2,col="blue",lty=2)
dev.off()


# 三阶多项式
pdf("多项式3阶.pdf", height = 6, width = 8, family = "GB1")
fit1=lm(CW~poly(D,3), data=larch)
summary(fit1)
# ,raw=T
xlims=range(D)
x.grid=seq(from=xlims[1],to=xlims[2])
preds=predict(fit1,newdata=list(D=x.grid),se=TRUE)
se.bands=cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit)
par(mar = c(4, 5.2, 2, 2), mgp = c(3, 1, 0))
plot(D,CW,xlim=xlims, col="darkgrey",xlab="胸径(cm)",ylab="冠幅(m)", cex.lab = 2.5, cex.axis = 2.5)
lines(x.grid,preds$fit,lwd=3,col="blue")
matlines(x.grid,se.bands,lwd=2,col="blue",lty=2)
dev.off()


# 五阶多项式
pdf("多项式5阶.pdf", height = 6, width = 8, family = "GB1")
fit1=lm(CW~poly(D,5), data=larch)
summary(fit1)
# ,raw=T
xlims=range(D)
x.grid=seq(from=xlims[1],to=xlims[2])
preds=predict(fit1,newdata=list(D=x.grid),se=TRUE)
se.bands=cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit)
par(mar = c(4, 5.2, 2, 2), mgp = c(3, 1, 0))
plot(D,CW,xlim=xlims, col="darkgrey",xlab="胸径(cm)",ylab="冠幅(m)", cex.lab = 2.5, cex.axis = 2.5)
lines(x.grid,preds$fit,lwd=3,col="blue")
matlines(x.grid,se.bands,lwd=2,col="blue",lty=2)
dev.off()

# y = x^2 + x^3
model.i <- lm(CW ~ I(D ^ 2) + I(D ^ 3), data = larch)

########## 2.回归样条模型 ##################
model.bs <- lm(CW ~ bs(D, df = 6), data = larch)
summary(model.bs)

########## 3.自然样条模型 ##################
model.ns <- lm(CW ~ ns(D, df = 4), data = larch)
summary(model.ns)
print( attr( ns(D, df = 4), "knots") )

########## 4.光滑样条模型 #################
model.ss = smooth.spline(D, CW, cv = TRUE)
print(model.ss)

########## 5.局部回归 ####################
model.lo1 = loess(CW ~ D, span = 0.1, data = larch)
model.lo5 = loess(CW ~ D, span = 0.5, data = larch)
summary(model.lo5)
