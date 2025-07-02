set.seed(123)

n <- 100
x <- seq(0, 10, length.out = n)

# 1. 无偏同方差：线性关系 + 常数方差 + 零均值误差
y1 <- 2 * x + rnorm(n, mean = 0, sd = 1)

# 2. 有偏同方差：非线性关系 + 常数方差
y2 <- 2 * x + 0.5 * x^2 + rnorm(n, mean = 0, sd = 1)

# 3. 无偏异方差：线性关系 + 方差随 x 增大
y3 <- 2 * x + rnorm(n, mean = 0, sd = 0.2 * x)

# 4. 有偏异方差：非线性关系 + 方差随 x 增大
y4 <- 2 * x + 0.5 * x^2 + rnorm(n, mean = 0, sd = 0.2 * x)

# 拟合线性模型并计算残差
residual_plot <- function(x, y, title) {
  model <- lm(y ~ x)
  residuals <- resid(model)
  fitted <- fitted(model)
  
  plot(fitted, residuals,
       main = title,
       xlab = "Fitted values",
       ylab = "Residuals",
       pch = 19, col = "black",
       cex.main = 1.5,  # 标题字体大小
       cex.lab = 1.5,   # 坐标轴标签字体大小
       cex.axis = 1.5   # 坐标轴刻度字体大小
       )
  abline(h = 0, col = "red", lwd = 2) 
}

# 绘图
pdf("D:\\大连理工大学\\R语言书稿\\Rforestat\\R_code(new)\\扩展\\残差图.pdf", width = 12, height = 8, family="GB1")
par(mfrow = c(2, 2), mgp = c(2.5, 1, 0))  # 2x2 图
residual_plot(x, y1, "无偏同方差")
residual_plot(x, y2, "有偏同方差")
residual_plot(x, y3, "无偏异方差")
residual_plot(x, y4, "有偏异方差")

dev.off()
