# 加载所需包
library(mgcv)

# 生成模拟数据集
set.seed(0)

dat <- gamSim(1, n = 200, scale = 2)

# 拟合广义加性模型
b <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat)

# 保存 gam.check 生成的诊断图为 PDF 文件
pdf("gam_check_plot.pdf", width = 8, height = 6, bg = "white")
par(mfrow = c(2, 2))
# 生成诊断图
gam.check(b, pch = 19, cex = .5)

dev.off()  # 关闭图像设备，保存文件
