# 定义激活函数
relu <- function(x) {
  pmax(0, x)  # ReLU函数
}

sigmoid <- function(x) {
  1 / (1 + exp(-x))  # Sigmoid函数
}

tanh_func <- function(x) {
  tanh(x)  # Tanh函数
}

# 创建 x 值的序列
x <- seq(-10, 10, by = 0.1)

# 计算对应的激活函数值
x.relu <- relu(x)
x.sigmoid <- sigmoid(x)
x.tanh <- tanh_func(x)

# 设置字体大小
font_size <- 2.5


# 绘制 ReLU 函数
pdf("图13.14a.pdf", width = 8, height = 8, family = "GB1")
par(mar = c(5, 5, 4, 2), mgp = c(3.5, 1, 0))
plot(x, x.relu, type = "l", col = "black", lwd = 2,
     cex.main = font_size,
     xlab = "x", ylab = "ReLU(x)", cex.lab = font_size, cex.axis = font_size, xlim = c(-10, 10),
     ylim = c(0, 10))  # 设置 y 轴范围
dev.off()

# 绘制 Sigmoid 函数
pdf("图13.14b.pdf", width = 8, height = 8, family = "GB1")
par(mar = c(5, 5, 4, 2), mgp = c(3.5, 1, 0))
plot(x, x.sigmoid, type = "l", col = "black", lwd = 2,
     cex.main = font_size,
     xlab = "x", ylab = "Sigmoid(x)", cex.lab = font_size, cex.axis = font_size, xlim = c(-10, 10),
     ylim = c(0, 1))  # 设置 y 轴范围
dev.off()

# 绘制 Tanh 函数
pdf("图13.14c.pdf", width = 8, height = 8, family = "GB1")
par(mar = c(5, 5.5, 4, 2), mgp = c(3.5, 1, 0))
plot(x, x.tanh, type = "l", col = "black", lwd = 2,
    cex.main = font_size,
     xlab = "x", ylab = "Tanh(x)", cex.lab = font_size, cex.axis = font_size, xlim = c(-10, 10),
     ylim = c(-1, 1))  # 设置 y 轴范围
dev.off()
