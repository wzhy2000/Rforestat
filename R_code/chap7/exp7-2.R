library(lmfor)

# 加载数据
data(spati2)
head(spati2)

#拟合线性模型
lm1<-gls(h~1,data = spati2)
lm1
mean(spati2$h)
sd(spati2$h)

#计算高度与直径关系绘制线性模型的散点图
spati2$y<-(spati2$d)/sqrt(spati2$h-1.3)
plot(spati2$d,spati2$y,pch=as.numeric(spati2$plot)%%25,cex=0.5,xlab = "d",ylab=expression(d/sqrt(h-1.3)),col=gray(0.7))
plots<-unique(spati2$plot)
for(i in 1:length(plots)){
  thisplot<-spati2[spati2$plot==plots[i],]
  model<-lm(y~d,data=thisplot)
  dvec<-seq(min(thisplot$d),max(thisplot$d),length=10)
  lines(dvec,coef(model)[1]+coef(model)[2]*dvec)}

#引入固定效应拟合模型
spati2$plot<-as.factor(spati2$plot)
lm2<-gls(h~plot-1,data=spati2)
coef(lm2)[1:2]
mean(spati2$h[spati2$plot==2])
mean(coef(lm2))
lm2$sigma


#引入随机效应拟合线性混合效应模型
lmm1<-lme(h~1,random = ~1|plot,data=spati2)
summary(lmm1)
ranef(lmm1)[1:5,]

#引入随机效应的拟合图
spati2$y<-(spati2$d)/sqrt(spati2$h-1.3)
lmm2<-lme(y~d,random = ~1|plot,data=spati2)
summary(lmm2)
ranef(lmm2)
fixef(lmm2)
plot(spati2$d,spati2$y,pch=as.numeric(spati2$plot)%%25,cex=0.5,xlab = "d",ylab=
         expression(d/sqrt(h-1.3)),col=gray(0.7))
linesplot(spati2$d,predict(lmm2),spati2$plot,add=TRUE,cex=0,col.lin = gray(0.2))
abline(fixef(lmm2),lwd=3)


#参数估计与假设检验
##拟合固定效应加随机效应
lmm3<-lme(y~d,random = ~d|plot,data = spati2)
summary(lmm3)
coef(lmm3)[1:3,]

#设置方差结构，进行假设检验
lmm4<-update(lmm3,weights=varPower(form = ~d))
lmm4
anova(lmm3,lmm4)


#置信区间提取
intervals(lmm4, which="fixed")



