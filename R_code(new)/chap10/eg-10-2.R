library(forestat)
library(MASS)
data("birch")

# 假设 birch 是包含胸径 D 和树高 H 的数据框
# 创建自启动函数
monomolecularInit <- function(mCall, LHS, data, ...) {
  xy <- sortedXyData(mCall[["D"]], LHS, data)
  ymax <- max(xy[["y"]], na.rm = TRUE)
  a.init <- 1.05 * ymax
  epsilon <- 1e-6
  log.data <- log(pmax(epsilon, (a.init - xy[["y"]]) / a.init))
  lmFit <- lm(log.data ~ xy[["x"]])
  b.init <- -unname(coef(lmFit)[2])
  value <- c(a = a.init, b = b.init)
  names(value) <- mCall[c("a", "b")]
  value
}

# 创建自启动模型
SSmono <- selfStart(
  model = ~ a * (1 - exp(-b * D)),  # 单分子式模型公式
  initial = monomolecularInit,       # 初始值函数
  parameters = c("a", "b")           # 参数名
)

# 使用 nls() 进行非线性最小二乘拟合
model.selfnls <- nls(H ~ SSmono(D, a, b), data = birch)

# 通过getInitial()函数获得模型的初始值
getInitial(H ~ SSmono(D, a, b), data = birch)


# 查看拟合结果
summary(model.selfnls)

model.nls <- nls(H ~ a * (1 - exp(-D)), data = birch, start = list(a = 1))
# 计算似然比统计量
logLik1 <- logLik(model.selfnls)
logLik0 <- logLik(model.nls)
LRT <- 2 * (as.numeric(logLik1) - as.numeric(logLik0))

# 计算自由度差
df <- attr(logLik1, "df") - attr(logLik0, "df")

# 计算 p 值
p.value <- pchisq(LRT, df = df, lower.tail = FALSE)

cat("p-value =", format.pval(
  p.value,
  digits = 3,
  eps = .Machine$double.eps
), "\n")


AIC(model.selfnls)
BIC(model.selfnls)

anova(model.selfnls, model.nls)

# 绘制残差图
pdf("Residuals.selfnls.pdf", width = 8, height = 8, family = "GB1")
par(mar = c(5, 5, 4, 2), mgp = c(3.2, 1, 0))
plot(fitted(model.selfnls), residuals(model.selfnls), xlab = "拟合值(m)", 
     ylab = "残差(m)", pch = 16, col = "black", cex = 1, 
     cex.lab = 2, cex.axis = 2)
abline(h = 0, col = "red")
dev.off()

profile.selfnls <- profile(model.selfnls)
pdf("selfnls.profile.pdf", width = 8, height = 6, family = "GB1")
par(mfrow = c(1, 2))
plot(profile.selfnls, cex.axis = 2, cex.lab = 2)
dev.off()

deriv3.formula <- deriv3(~ a * (1 - exp(-b * D)), 
                           c("a", "b"), 
                           function(a, b, D) NULL)

model.deriv3 <- nls(H ~ deriv3.formula(a, b, D), data = birch, start = list(a = 20.7, b = 0.03874049
))
rms.curv(model.deriv3)
