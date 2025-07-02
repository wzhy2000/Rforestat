library(lmfor)
library(nlme)
#加载数据
data("BrkRes")
head(BrkRes)

#拟合线性混合效应模型brmod1（去掉截距项）
model.tree <- lme(Resistance ~ RingClass + Density, random = ~ RingClass - 1 | Tree, data = BrkRes)
model.tree

#绘制brmod1模型观测值和预测值
aug.pred <- augPred(model.tree, primary = ~ Density, level = 0:1, length.out = 2)
plot(aug.pred, layout = c(4, 4, 1),
     key = list(lines = list(lty = c(1, 2)), 
                text = list(c("Marginal", "Subject-specific")),
                columns = 2))

#拟合线性混合效应模型brmod2
model.ring.tree <- lme(Resistance ~ RingClass + Density, random = ~RingClass | Tree, data = BrkRes)
model.ring.tree  

qqnorm(model.ring.tree, ~resid(.) | RingClass)


#拟合线性混合效应模型brmod3进行似然比检验
model.null.tree <- lme (Resistance ~ RingClass + Density, random = ~1 | Tree, data = BrkRes)
lr.test <- anova(model.ring.tree, model.null.tree)
print(lr.test)




