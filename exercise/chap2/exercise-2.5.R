# 生成100个标准正态分布数据
set.seed(1)
data <- rnorm(100)
setwd("D:/你的路径")  # 改成你想要保存的文件夹路径
# 打开PNG图形设备，设置宽高与分辨率
png("histogram.png", width = 800, height = 600, res = 100)

# 绘图（不使用中文，避免编码问题）
hist(data,
     col = "lightblue",
     main = "Histogram of N(0,1)",
     xlab = "Value",
     ylab = "Frequency",
     border = "darkgray")

# 关闭图形设备，保存图像
dev.off()
