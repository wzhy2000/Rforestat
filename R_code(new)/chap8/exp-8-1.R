library(lmfor)

# 加载数据
data(spati2)
summary(spati2)

#拟合线性模型
model.gls.null <- gls(h ~ 1, data = spati2)
model.gls.null
mean(spati2$h)
sd(spati2$h)

spati2$y <- (spati2$d)/sqrt(spati2$h - 1.3)
pdf("图7.3a.pdf", width = 8, height = 6, family = "GB1")
par(mar = c(5, 5.5, 4, 2), mgp = c(3.5, 1, 0))
plot(spati2$d, spati2$y, pch = as.numeric(spati2$plot) %% 25, cex = 0.5, xlab = "胸径(cm)", 
     ylab = expression(d/sqrt(h-1.3)), col = gray(0.7), cex.axis = 2.2, cex.lab = 2.2)#计算高度与直径关系绘制线性模型的散点图
plots <- unique(spati2$plot)
for(i in 1:length(plots)){
  thisplot <- spati2[spati2$plot == plots[i], ]
  model <- lm(y ~ d,data = thisplot)
  dvec <- seq(min(thisplot$d), max(thisplot$d), length = 10)
  lines(dvec, coef(model)[1] + coef(model)[2]  *dvec)}
dev.off()

#引入固定效应拟合模型
spati2$plot<-as.factor(spati2$plot)
model.gls.plot <- gls(h ~ plot - 1, data = spati2)
coef(model.gls.plot)[1:2]
mean(spati2$h[spati2$plot==2])
mean(coef(model.gls.plot))
model.gls.plot$sigma


#线性混合效应模型构建
model.plot <- lme(h ~ 1, random = ~1 | plot, data = spati2)
summary(model.plot)

spati2$y <- spati2$d / sqrt(spati2$h - 1.3)
model.d.plot <- lme(y ~ d, random = ~1 | plot, data = spati2)
summary(model.d.plot)


ranef(model.d.plot)
fixef(model.d.plot)
pdf("图7.3b.pdf", width = 8, height = 6, family = "GB1")
par(mar = c(5, 5.5, 4, 2), mgp = c(3.5, 1, 0))
plot(spati2$d,spati2$y,pch=as.numeric(spati2$plot)%%25,cex=0.5,xlab = "胸径(cm)",ylab=
         expression(d/sqrt(h-1.3)),col=gray(0.7), cex.lab = 2.2, cex.axis = 2.2)#引入随机效应的拟合图
linesplot(spati2$d,predict(model.d.plot),spati2$plot,add=TRUE,cex=0,col.lin = gray(0.2))
abline(fixef(model.d.plot),lwd=3)
dev.off()


#拟合固定效应加随机效应
model.d.d <- lme(y ~ d, random = ~d | plot, data = spati2)
summary(model.d.d)



#参数估计
ranef_value<-ranef(model.d.d)
head(ranef_value, n=3)
fixef(model.d.d)
coef(model.d.d)[1:3,]


#设置方差结构，进行置信区间

model.d.d2 <- update(model.d.d, weights = varPower(form = ~d))    
intervals(model.d.d2, which = "fixed")
intervals(model.d.d2, which = "var-cov")


#模型比较
model.d.d2 <- update(model.d.d, weights = varPower(form = ~d))
anova(model.d.d, model.d.d2)






