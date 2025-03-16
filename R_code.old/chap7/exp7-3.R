library(lmfor)
#加载数据
data("BrkRes")
head(BrkRes)

#拟合线性混合效应模型brmod1（去掉截距项）
brmod1<-lme(Resistance ~ RingClass + Density,
            random= ~ RingClass-1|Tree,
            data = BrkRes)
brmod1

#绘制brmod1模型观测值和预测值
aug.pred<-augPred(brmod1,primary = ~Density,level=0:1,length.out = 2)
plot(aug.pred,layout=c(4,4,1),
       key=list(lines=list(lty=c(1,2)),
                text=list(c("Marginal","Subject-specific")),
                columns=2))

#拟合线性混合效应模型brmod2
brmod2<-lme(Resistance ~ RingClass + Density,
            random = ~ RingClass|Tree,
            data = BrkRes)
brmod2          
qqnorm(brmod2,~resid(.)|RingClass)


#拟合线性混合效应模型brmod3进行似然比检验
brmod3<-lme(Resistance ~ RingClass + Density,
            random = ~ 1|Tree,
            data = BrkRes)
lr_test<-anova(brmod2,brmod3)
print(lr_test)




