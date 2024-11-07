

library(ggplot2)
library(grid)  # 用于定义箭头

# 创建数据
x <- seq(-4, 4, length.out = 100)
y <- dnorm(x, mean = 0, sd = 1)

# 绘制正态分布图并添加一倍、二倍、三倍方差范围
pdf("4.1正态分布.pdf",width = 8, height = 4, family = "GB1")
ggplot(data.frame(x, y), aes(x = x, y = y)) +
  geom_line(color = "black") +
  geom_vline(xintercept = c(-1, 1), linetype = "dotted", color = "black", size = 1) +  # 一倍方差
  geom_vline(xintercept = c(-2, 2), linetype = "dotted", color = "black", size = 1) +  # 二倍方差
  theme_classic() +
  labs(x = "x", y = "概率密度") +
  annotate("text", x = 0, y = 0.2, label = "±1σ", vjust = -1, size = 5, color = "black") +
  annotate("text", x = 0, y = 0.48, label = "±2σ", vjust = -1, size = 5, color = "black") +
  geom_segment(aes(x = -1, xend = 1, y = 0.15, yend = 0.15), 
               color = "black", size = 1,
               arrow = arrow(angle = 15, length = unit(0.1, "inches"), ends = "both", type = "closed")) +  # 68% 范围线双箭头
  geom_segment(aes(x = -2, xend = 2, y = 0.5, yend = 0.5), 
               color = "black", size = 1,
               arrow = arrow(angle = 15, length = unit(0.1, "inches"), ends = "both", type = "closed")) +   # 95% 范围线双箭头
  coord_cartesian(ylim = c(0, 0.6)) +   # 设置y轴范围
  theme(plot.title = element_text(size = 16),   # 设置标题字体大小
        axis.title = element_text(size = 24),   # 设置坐标轴标签字体大小
        axis.text = element_text(size = 24),    # 设置坐标轴刻度字体大小
        legend.title = element_text(size = 28), # 设置图例标题字体大小
        legend.text = element_text(size = 24)   # 设置图例文本字体大小
  )
dev.off()


library(ggplot2)

# 创建数据
x <- 0:15
data <- data.frame(
  x = rep(x, 3),  # 重复 x 值三次用于绘制不同的 λ
  y = c(dpois(x, lambda = 1), dpois(x, lambda = 2), dpois(x, lambda = 3)),  # 计算三条泊松分布的 y 值
  lambda = factor(rep(c(1, 2, 3), each = length(x)))  # 用于区分三条曲线的 λ 值
)

# 绘制泊松分布图
pdf("4.2泊松分布.pdf",width = 8, height = 4, family = "GB1")

ggplot(data, aes(x = x, y = y, linetype = lambda, group = lambda)) +
  geom_line(size = 1) +  # 绘制曲线
  geom_point(size = 1) +  # 添加数据点
  theme_classic() +
  labs(x = "x", y = "概率质量", linetype = "λ") +  # 添加图例标题
  scale_linetype_manual(values = c("solid", "dashed", "dotted"))  + # 自定义线型类型
  theme(plot.title = element_text(size = 16),   # 设置标题字体大小
        axis.title = element_text(size = 24),   # 设置坐标轴标签字体大小
        axis.text = element_text(size = 24),    # 设置坐标轴刻度字体大小
        legend.title = element_text(size = 28), # 设置图例标题字体大小
        legend.text = element_text(size = 24)   # 设置图例文本字体大小
  )
dev.off()

library(ggplot2)

# 创建数据
x <- seq(0, 2.5, length.out = 100)
data <- data.frame(
  x = rep(x, 3),  # 重复 x 值三次用于绘制不同的 shape
  y = c(dweibull(x, shape = 1.5, scale = 1), 
        dweibull(x, shape = 2, scale = 1), 
        dweibull(x, shape = 3, scale = 1)),  # 计算三条 Weibull 分布的 y 值
  shape = factor(rep(c(1.5, 2, 3), each = length(x)))  # 用于区分三条曲线的 shape 参数
)

# 绘制Weibull分布图
pdf("4.3weibull分布.pdf",width = 8, height = 4, family = "GB1")
ggplot(data, aes(x = x, y = y, linetype = shape, group = shape)) +
  geom_line(size = 1) +  # 绘制曲线
  theme_classic() +
  labs(x = "x", y = "概率密度", linetype = "k") +  # 添加图例标题
  scale_linetype_manual(values = c("solid", "dashed", "dotted"))  + # 自定义线型类型
  theme(plot.title = element_text(size = 16),   # 设置标题字体大小
        axis.title = element_text(size = 24),   # 设置坐标轴标签字体大小
        axis.text = element_text(size = 24),    # 设置坐标轴刻度字体大小
        legend.title = element_text(size = 28), # 设置图例标题字体大小
        legend.text = element_text(size = 24)   # 设置图例文本字体大小
  )
dev.off()

# 创建数据
library(ggplot2)

# 创建数据
x <- seq(0, 15, length.out = 100)
data_chisq <- data.frame(
  x = rep(x, 3),  # 重复 x 值三次用于绘制不同的自由度
  y = c(dchisq(x, df = 3), dchisq(x, df = 5), dchisq(x, df = 7)),  # 计算三条卡方分布的 y 值
  df = factor(rep(c(3, 5, 7), each = length(x)))  # 用于区分不同自由度的曲线
)

# 绘制卡方分布图
pdf("4.4卡方分布.pdf",width = 8, height = 4, family = "GB1")
ggplot(data_chisq, aes(x = x, y = y, linetype = df, group = df)) +
  geom_line(size = 1) +  # 绘制曲线
  theme_classic() +
  labs(x = "x", y = "概率密度", linetype = "d") +
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) + # 使用不同线型区分
  theme(plot.title = element_text(size = 16),   # 设置标题字体大小
        axis.title = element_text(size = 24),   # 设置坐标轴标签字体大小
        axis.text = element_text(size = 24),    # 设置坐标轴刻度字体大小
        legend.title = element_text(size = 28), # 设置图例标题字体大小
        legend.text = element_text(size = 24)   # 设置图例文本字体大小
  )
dev.off()


library(ggplot2)

# 创建数据
x <- seq(-4, 4, length.out = 100)
data_t <- data.frame(
  x = rep(x, 3),  # 重复 x 值三次用于绘制不同的自由度
  y = c(dt(x, df = 0.5), dt(x, df = 1), dt(x, df = 50)),  # 计算三条 t 分布的 y 值
  df = factor(rep(c(0.5, 1, 50), each = length(x)))  # 用于区分不同自由度的曲线
)

# 绘制t分布图
pdf("4.5t分布.pdf",width = 8, height = 4, family = "GB1")
ggplot(data_t, aes(x = x, y = y, linetype = df, group = df)) +
  geom_line(size = 1) +  # 绘制曲线
  theme_classic() +
  labs(x = "t", y = "概率密度", linetype = "d") +
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) + # 使用不同线型区分
  theme(plot.title = element_text(size = 16),   # 设置标题字体大小
        axis.title = element_text(size = 24),   # 设置坐标轴标签字体大小
        axis.text = element_text(size = 24),    # 设置坐标轴刻度字体大小
        legend.title = element_text(size = 28), # 设置图例标题字体大小
        legend.text = element_text(size = 24)   # 设置图例文本字体大小
        )
dev.off()


# 创建数据
x <- seq(0, 5, length.out = 100)
y <- df(x, df1 = 5, df2 = 2)

# 绘制F分布图
pdf("4.6F分布.pdf",width = 8, height = 4, family = "GB1")
ggplot(data.frame(x, y), aes(x = x, y = y)) +
  geom_line(color = "black") +
  theme_classic() +
  labs(x = "x", y = "概率密度") + # 使用不同线型区分
  theme(plot.title = element_text(size = 16),   # 设置标题字体大小
        axis.title = element_text(size = 24),   # 设置坐标轴标签字体大小
        axis.text = element_text(size = 24),    # 设置坐标轴刻度字体大小
        legend.title = element_text(size = 28), # 设置图例标题字体大小
        legend.text = element_text(size = 24)   # 设置图例文本字体大小
  )
dev.off()

