library(forestat)
data("birch")

# 假设 birch 是包含胸径 D 和树高 H 的数据框
# 创建自启动函数
monomolecularInit <- function(mCall, LHS, data, ...) {
  # 提取胸径 D 和树高 H 数据
  xy <- sortedXyData(mCall[["D"]], LHS, data)  # 确保数据按 D 排序
  
  a.init <- max(xy[, "y"], na.rm = TRUE)  # a 设为 y 的最大值
  # 线性拟合的初步估计
  epsilon <- 10e-6
  # 确保 log 内的值为正且为数值型
  log.data <- log(pmax(epsilon, (max(xy[, "y"]) - xy[, "y"]) / max(xy[, "y"])))
  lmFit <- lm(log.data ~ xy[, "x"])  # 拟合对数转换后的数据
  coefs <- coef(lmFit)
  
  b.init <- -coef(lmFit)[2]  # b 从线性拟合的斜率计算得到
  
  # 返回初始值
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
LTR <- 2 * (logLik1 - logLik0)

# 计算自由度差
df <- df.residual(model.nls) - df.residual(model.selfnls)

# 计算 p 值
p.value <- 1 - pchisq(LTR, df)
print(p.value)


AIC(model.selfnls)
BIC(model.selfnls)

anova(model.selfnls, model.nls)

# 绘制残差图
pdf("Residuals.selfnls.pdf", width = 8, height = 8, family = "GB1")
par(mar = c(5, 5, 4, 2), mgp = c(3.5, 1, 0))
plot(fitted(model.selfnls), residuals(model.selfnls), xlab = "拟合值", 
     ylab = "残差", pch = 16, col = "black", cex = 1, 
     cex.lab = 2, cex.axis = 2)
abline(h = 0, col = "red")
dev.off()

profile.selfnls <- profile(model.selfnls)
pdf("selfnls.profile.pdf", width = 8, height = 6, family = "GB1")
par(mfrow = c(1, 2))
plot(profile.selfnls)
dev.off()

deriv3.formula <- deriv3(~ a * (1 - exp(-b * D)), 
                           c("a", "b"), 
                           function(a, b, D) NULL)

model.deriv3 <- nls(H ~ deriv3.formula(a, b, D), data = birch, start = list(a = 20.7, b = 0.03874049
))
rms.curv(model.deriv3)
