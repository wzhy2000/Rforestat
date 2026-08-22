# 本实验仍基于例5.1的杨树栽培试验数据，使用实验5.1中建立的fit2（仅包含苗高作为自变量）模型进行诊断图的绘制并分析。

# 删除环境中的对象
rm(list = ls())

library(forestat)

poplar <- read.csv("../../data/case-6.1.csv", sep = ",")

model.x4 <- lm(y ~ x4, data = poplar)
summary(model.x4)

#pdf("diagnosisgraph.pdf")
par(mfrow = c(2, 2), cex = 1.1, cex.axis = 1.1, cex.lab = 1.1, cex.main = 1.1)
plot(model.x4)
#dev.off()
















