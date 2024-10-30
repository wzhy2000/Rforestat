# 加载所需包
library(mgcv)

# 生成模拟数据集
n <- 200
sig <- 2
dat <- gamSim(1, n = n, scale = sig)

# 拟合广义加性模型
b <- gam(y ~ s(x0) + s(I(x1^2)) + s(x2) + offset(x3), data = dat)

# 生成新数据集，用于预测
newd <- data.frame(
  x0 = (0:30) / 30, 
  x1 = (0:30) / 30, 
  x2 = (0:30) / 30, 
  x3 = (0:30) / 30 # 注意：这里确保 x3 是一个有效的数值列
)

# 生成预测值
pred <- predict.gam(b, newd)

# 生成不包含 s(x0) 的预测值
pred0 <- predict(b, newd, exclude = "s(x0)")

# 保存图像为黑白调，适合书籍打印
png("predict_plot_bw.png", width = 800, height = 600, bg = "white")  # 背景设为白色

# 绘制黑白线条图，横纵坐标为英文
plot(newd$x0, pred, type = "l", col = "black", lwd = 2,
     main = "Prediction Results", xlab = "x0", ylab = "Predicted Values", 
     col.axis = "black", col.lab = "black", col.main = "black")

dev.off()