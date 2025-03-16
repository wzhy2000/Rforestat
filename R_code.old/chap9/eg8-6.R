library(forestat)
library(mgcv)
library(gamreg)

data("larch")
attach(larch)

# 利用胸径（D）和枝下高（CBH）构建树高模型
model <- gam(H ~ s(D) + s(CBH) + te(D, CBH), data = larch, method="REML", select = TRUE)
summary(model)

pdf("smooth_effect.pdf",width = 8,height = 6)
plot(model,pages=1)
dev.off()

pdf("check.pdf",width = 8,height = 6)
par(mfrow = c(2, 2))
gam.check(model, pch = 19, cex = .5)
dev.off()

Y <- as.matrix(H)  # 响应变量：树高
X <- as.matrix(larch[, c("D", "CBH")])  # 预测变量矩阵，包括胸径和枝下高

# 运行交叉验证
res <- cv.gam(
  X = X,
  Y = Y,
  nlambda = 10,        # 惩罚参数网格数
  init.mode = "sLTS",  # 初始化方法
  fold = 5,            # 5 折交叉验证
  lmin = 0.01,         # 惩罚参数的最小值
  lmax = 1             # 惩罚参数的最大值
)
res$lambda
res$Rocv

