# eg5-1
library("openxlsx")
data1=read.csv("data-{30m}-5-3.csv")
fit1=lm(y~1+x2+x3+x4+x5,data=data1)
fit1
summary(fit1)
names(fit1)
round(fit1$coefficients,4)
round(coef(fit1),4)
round(fit1$residuals,4)
round(resid(fit1), 4)
confint(fit1)
predict(fit1,data.frame(x2=1,x3=-1,x4=-1,x5=5.0),interval="prediction")
predict(fit1,data.frame(x2=1,x3=-1,x4=-1,x5=5.0),interval="confidence")

fit2=lm(y~x5,data=data1)
summary(fit2)

pdf("predandconfi.pdf")  
attach(data1)
#计算置信区间和预测区间
newdata=data.frame(x5=seq(2,9,by=0.25))
pp=predict(fit2,newdata,interval="prediction")
pc=predict(fit2,newdata,interval="confidence")
# 绘制拟合线  
matplot(newdata$x5,cbind(pp,pc[,-1]),type="l",xlab="height",ylab="increment",lty=c(1,5,5,2,2),col=c("blue","red","red","black","black"),lwd=2)
points(x5,y,cex=2,pch=20)
#作出图例
legend("topleft",c("Points","Fit","Prediction","Confidence"),pch=c(19,NA,NA,NA),lty=c(NA,1,5,2),lwd=c(NA,2,2,2),col=c("black","blue","red","black"))
dev.off()

# eg5-2
par(mfrow=c(2,2))
plot(fit2)


