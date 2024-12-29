library(gridExtra)
library(ggplot2)
library(grid)  # 用于定义箭头

##################### 1.非标准正态分布##########################
# 创建x值的范围
x <- seq(-10, 15, by = 0.01)
params <- data.frame(mu = c(2, 2, 3), sd = c(2, 3, 2))  # 指定自由度组合
df <- do.call(rbind, lapply(1:nrow(params), function(i) {
  mu <- params$mu[i]
  sd <- params$sd[i]
  y <- dnorm(x, mean = mu, sd = sd)
  data.frame(x = x, y = y, mu = mu, sd = sd)
}))
df$group <- factor(paste("mu=", df$mu, ", sd=", df$sd))

set.seed(123)
# 绘图
p1a <- ggplot(df, aes(x = x, y = y, linetype = group, group = group)) +
  geom_line(color = "black") +
  theme_classic() +
  labs(x = "x", y = "概率密度", linetype = "mu, sd") +
  ggtitle("非标准正态分布") +
  scale_y_continuous(limits = c(0, 0.25), expand = c(0, 0)) +   # 设置y轴范围
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) + # 使用不同线型区分
  theme(plot.title = element_text(size = 24),   # 设置标题字体大小
        axis.title = element_text(size = 20),   # 设置坐标轴标签字体大小
        axis.text = element_text(size = 20),    # 设置坐标轴刻度字体大小
        legend.title = element_text(size = 28), # 设置图例标题字体大小
        legend.text = element_text(size = 24),   # 设置图例文本字体大小
        legend.position = c(0.8, 0.8),          # 将图例移到图内，右上角 (80% x, 80% y)
  )
p1a

mu <- 2  # 均值
sigma <- 3  # 标准差
# 创建x值的范围
x <- seq(mu - 4*sigma, mu + 4*sigma, by = 0.01)
# 计算y值，使用非标准正态分布的公式
set.seed(123)
y <- dnorm(x, mean = mu, sd = sigma)
df <- data.frame(x = x, y = y)
# 定义 alpha
alpha <- 0.05  # 例如 0.05 显著性水平
# 计算上 alpha 分位数
x_alpha <- qnorm(1 - alpha, mean = mu, sd = sigma)

p1b <- ggplot(df, aes(x = x, y = y)) +
  geom_line(color = "black") + 
  theme_classic() + 
  geom_area(data = subset(df, x >= x_alpha), aes(x = x, y = y), fill = "black", alpha = 0.3) + # 添加阴影部分
  labs(x = "x", y = "概率密度") +
  ggtitle("非标准正态分布") +
  annotate("segment", x = x_alpha + 1, xend = x_alpha + 2, y = dnorm(x_alpha + 1, mean = mu, sd = sigma) - 0.01, yend = 0.03,  # 添加指向阴影部分的箭头
           color = "black", arrow = arrow(angle = 15, length = unit(0.2, "inches"), type = "closed")) + 
  annotate("text", 
           x = x_alpha + 2.7, y = 0.03,   # 设置文字的位置，略偏离箭头终点
           label = expression(alpha),    # 使用数学符号 \(\alpha\)
           size = 10, color = "black") + 
  annotate("segment", x = x_alpha, xend = x_alpha, y = 0, yend = dnorm(x_alpha, mean = mu, sd = sigma),
           color = "red", size = 1) + 
  scale_y_continuous(limits = c(0, 0.25), expand = c(0, 0)) +   # 设置y轴范围
  scale_x_continuous(
    breaks = c(seq(-10, 15, by = 5), x_alpha),
    labels = parse(text = paste0(sapply(c(seq(-10, 15, by = 5), x_alpha), function(val) {
      if(abs(val - x_alpha) < 0.0001) {
        "x[alpha]"  # 修改为卡方分布的表达式
      } else {
        val
      }
    })))
  ) + 
  theme(plot.title = element_text(size = 24),   # 设置标题字体大小
        axis.title = element_text(size = 20),   # 设置坐标轴标签字体大小
        axis.text = element_text(size = 20),    # 设置坐标轴刻度字体大小
        legend.title = element_text(size = 28), # 设置图例标题字体大小
        legend.text = element_text(size = 24),   # 设置图例文本字体大小
  ) 
p1b

############################## 2.标准正态分布############################
# 创建数据
x <- seq(-4, 4, by = 0.01)
y <- dnorm(x, mean = 0, sd = 1)
a <- 0.95
x_a <- qnorm(a)
y_a <- dnorm(x_a, mean = 0, sd = 1)
df <- data.frame(x = x, y = y)

# 绘制正态分布图并添加一倍、二倍方差范围
p2a <-  ggplot(df, aes(x = x, y = y)) +
  geom_line(color = "black") + 
  geom_vline(xintercept = c(-1, 1), linetype = "dotted", color = "black", size = 1) +  # 一倍方差
  geom_vline(xintercept = c(-2, 2), linetype = "dotted", color = "black", size = 1) +  # 二倍方差
  theme_classic() +
  labs(x = "x", y = "概率密度") +
  ggtitle("标准正态分布") +
  annotate("text", x = 0, y = 0.15, label = "±1σ", vjust = -1, size = 5, color = "black") +
  annotate("text", x = 0, y = 0.5, label = "±2σ", vjust = -1, size = 5, color = "black") +
  annotate("segment", x = -1, xend = 1, y = 0.15, yend = 0.15, 
           color = "black", size = 1,
           arrow = arrow(angle = 15, length = unit(0.1, "inches"), ends = "both", type = "closed")) +  # 68% 范围线双箭头
  annotate("segment", x = -2, xend = 2, y = 0.5, yend = 0.5, 
           color = "black", size = 1,
           arrow = arrow(angle = 15, length = unit(0.1, "inches"), ends = "both", type = "closed")) +   # 95% 范围线双箭头
  coord_cartesian(ylim = c(0, 0.6)) +   # 设置y轴范围
  scale_y_continuous(expand = c(0, 0)) + 
  theme(plot.title = element_text(size = 24),   # 设置标题字体大小
        axis.title = element_text(size = 20),   # 设置坐标轴标签字体大小
        axis.text = element_text(size = 20),    # 设置坐标轴刻度字体大小
        legend.title = element_text(size = 28), # 设置图例标题字体大小
        legend.text = element_text(size = 24),   # 设置图例文本字体大小
        plot.margin = margin(t = 5, r = 5, b = 20, l = 5)
  )
p2a
# dev.off()



# pdf("4.1正态分布.pdf",width = 8, height = 4, family = "GB1")
p2b <-  ggplot(df, aes(x = x, y = y)) +
  geom_line(color = "black") +
  geom_area(data = subset(df, x >= x_a), aes(x = x, y = y), fill = "black", alpha = 0.3) +
  theme_classic() +
  labs(x = "x", y = "概率密度") +
  ggtitle("标准正态分布") + 
  annotate("segment", x = x_a + 0.5, xend = x_a + 1, y = dnorm(x_a + 0.5, mean = 0, sd = 1) - 0.01, yend = 0.1,  # 添加指向阴影部分的箭头
           color = "black", arrow = arrow(angle = 15, length = unit(0.2, "inches"), type = "closed")) + 
  annotate("text", 
           x = x_a + 1.2, y = 0.11,   # 设置文字的位置，略偏离箭头终点
           label = expression(alpha),    # 使用数学符号 \(\alpha\)
           size = 10, color = "black") + 
  annotate("segment", x = x_a, xend = x_a, y = 0, yend = dnorm(x_a, mean = 0, sd = 1),
           color = "red", size = 1) + # 上alpha分位数线
  coord_cartesian(ylim = c(0, 0.6)) +   # 设置y轴范围
  scale_y_continuous(expand = c(0, 0)) + 
  scale_x_continuous(
    breaks = c(seq(-4, 4, by = 2), x_a),
    labels = parse(text = paste0(sapply(c(seq(-4, 4, by = 2), x_a), function(val) {
      if(abs(val - x_a) < 0.0001) {
        "z[alpha]"  # 修改为卡方分布的表达式
      } else {
        val
      }
    })))
  ) +
  theme(plot.title = element_text(size = 24),   # 设置标题字体大小
        axis.title = element_text(size = 20),   # 设置坐标轴标签字体大小
        axis.text = element_text(size = 20),    # 设置坐标轴刻度字体大小
        legend.title = element_text(size = 28), # 设置图例标题字体大小
        legend.text = element_text(size = 24),   # 设置图例文本字体大小
        plot.margin = margin(t = 5, r = 5, b = 20, l = 5)
  )
p2b
# dev.off()

############################ 3.t分布 ########################################
library(ggplot2)

# 创建数据
x <- seq(-10, 10, by = 0.01)
data_t <- data.frame(
  x = rep(x, 3),  # 重复 x 值三次用于绘制不同的自由度
  y = c(dt(x, df = 0.5), dt(x, df = 1), dt(x, df = 50)),  # 计算三条 t 分布的 y 值
  df = factor(rep(c(0.5, 1, 50), each = length(x)))  # 用于区分不同自由度的曲线
)

# 绘制t分布图
# pdf("4.5t分布.pdf",width = 8, height = 4, family = "GB1")
p3a <- ggplot(data_t, aes(x = x, y = y, linetype = df, group = df)) +
  geom_line(size = 1) +  # 绘制曲线
  theme_classic() +
  labs(x = "t", y = "概率密度", linetype = "d") +
  ggtitle("t分布") +
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) + # 使用不同线型区分
  coord_cartesian(ylim = c(0, 0.45)) +   # 设置y轴范围
  scale_y_continuous(expand = c(0, 0)) +
  theme(plot.title = element_text(size = 24),   # 设置标题字体大小
        axis.title = element_text(size = 20),   # 设置坐标轴标签字体大小
        axis.text = element_text(size = 20),    # 设置坐标轴刻度字体大小
        legend.title = element_text(size = 28), # 设置图例标题字体大小
        legend.text = element_text(size = 24),   # 设置图例文本字体大小
        legend.position = c(0.8, 0.8),          # 将图例移到图内，右上角 (80% x, 80% y)
  )
p3a
# dev.off()

a <- 0.95
x_a <- qt(a, df = 5)
x <- seq(-6, 6, by = 0.01)
data <- data.frame(x = x, y = dt(x, df = 5))

# 绘制t分布图
# pdf("4.5t分布.pdf",width = 8, height = 4, family = "GB1")
p3b <- ggplot(data, aes(x = x, y = y)) +
  geom_line(size = 1) +  # 绘制曲线
  theme_classic() +
  labs(x = "t", y = "概率密度") +
  ggtitle("t分布") + 
  geom_area(data = subset(data, x >= x_a), aes(x = x, y = y), fill = "black", alpha = 0.3) +
  annotate("segment", x = x_a, xend = x_a, y = 0, yend = dt(x_a, df = 5),
           color = "red", size = 1) + # 上alpha分位数线
  annotate("segment", x = x_a + 1, xend = x_a + 2, y = dt(x_a + 1, df = 5) - 0.005, yend = 0.05,  # 添加指向阴影部分的箭头
           color = "black", arrow = arrow(angle = 15, length = unit(0.2, "inches"), type = "closed")) +
  annotate("text",
           x = x_a + 2.25, y = 0.06,   # 设置文字的位置，略偏离箭头终点
           label = expression(alpha),    # 使用数学符号 \(\alpha\)
           size = 10, color = "black") +
  coord_cartesian(ylim = c(0, 0.4)) +   # 设置y轴范围
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(
    breaks = c(seq(-6, 6, by = 2), x_a),
    labels = parse(text = paste0(sapply(c(seq(-6, 6, by = 2), x_a), function(val) {
      if(abs(val - x_a) < 0.0001) {
        "t[alpha]"  # 修改为卡方分布的表达式
      } else {
        val
      }
    })))
  ) +
  theme(plot.title = element_text(size = 24),   # 设置标题字体大小
        axis.title = element_text(size = 20),   # 设置坐标轴标签字体大小
        axis.text = element_text(size = 20),    # 设置坐标轴刻度字体大小
        legend.title = element_text(size = 28), # 设置图例标题字体大小
        legend.text = element_text(size = 24),   # 设置图例文本字体大小
  )
p3b



################################ 4. 泊松分布 ######################### 
library(ggplot2)
# 创建数据
x <- 0:10
data <- data.frame(
  x = rep(x, 3),  # 重复 x 值三次用于绘制不同的 λ
  y = c(dpois(x, lambda = 1), dpois(x, lambda = 2), dpois(x, lambda = 3)),  # 计算三条泊松分布的 y 值
  lambda = factor(rep(c(1, 2, 3), each = length(x)))  # 用于区分三条曲线的 λ 值
)

# 绘制泊松分布图
# pdf("4.2泊松分布.pdf",width = 8, height = 4, family = "GB1")
p4a <- ggplot(data, aes(x = x, y = y, linetype = lambda, group = lambda)) +
  geom_line(size = 1) +  # 绘制曲线
  geom_point(size = 1) +  # 添加数据点
  theme_classic() +
  labs(x = "x", y = "概率密度", linetype = "λ") +  # 添加图例标题
  ggtitle("泊松分布") +
  scale_linetype_manual(values = c("solid", "dashed", "dotted"))  + # 自定义线型类型
  coord_cartesian(ylim = c(0, 0.4)) +   # 设置y轴范围
  scale_y_continuous(expand = c(0, 0)) +
  theme(plot.title = element_text(size = 24),   # 设置标题字体大小
        axis.title = element_text(size = 20),   # 设置坐标轴标签字体大小
        axis.text = element_text(size = 20),    # 设置坐标轴刻度字体大小
        legend.title = element_text(size = 28), # 设置图例标题字体大小
        legend.text = element_text(size = 24),   # 设置图例文本字体大小
        legend.position = c(0.8, 0.8),          # 将图例移到图内，右上角 (80% x, 80% y)
  )
p4a
# dev.off()


a <- 0.95
x_a <- qpois(a, lambda = 2)
y_a <- dpois(x_a, lambda = 2)
x <- 0:10
df <- data.frame(x = x, y = dpois(x, lambda = 2), lambda = factor(rep(2, each = length(x))))
p4b <- ggplot(df, aes(x = x, y = y)) +
  geom_line(size = 1) +  # 绘制曲线
  geom_point(size = 1) +  # 添加数据点
  theme_classic() +
  labs(x = "x", y = "概率密度") +  # 添加图例标题
  geom_area(data = subset(df, x >= x_a), aes(x = x, y = y), fill = "black", alpha = 0.3) +
  ggtitle("泊松分布") + 
  annotate("segment", x = x_a, xend = x_a, y = 0, yend = dpois(x_a, lambda = 2),
           color = "red", size = 1) + # 上alpha分位数线
  annotate("segment", x = x_a + 0.5, xend = x_a + 1.3, y = dpois(x_a, lambda = 2) - 0.02, yend = 0.05,  # 添加指向阴影部分的箭头
           color = "black", arrow = arrow(angle = 15, length = unit(0.2, "inches"), type = "closed")) + 
  annotate("text", 
           x = x_a + 1.55, y = 0.05,   # 设置文字的位置，略偏离箭头终点
           label = expression(alpha),    # 使用数学符号 \(\alpha\)
           size = 10, color = "black") + 
  coord_cartesian(ylim = c(0, 0.35)) +   # 设置y轴范围
  scale_y_continuous(expand = c(0, 0)) + 
  scale_x_continuous(
    breaks = c(seq(0, 10, by = 2), x_a),
    labels = parse(text = paste0(sapply(c(seq(0, 10, by = 2), x_a), function(val) {
      if(abs(val - x_a) < 0.0001) {
        "lambda[alpha]"  # 修改为卡方分布的表达式
      } else {
        val
      }
    })))
  )  +
  theme(plot.title = element_text(size = 24),   # 设置标题字体大小
        axis.title = element_text(size = 20),   # 设置坐标轴标签字体大小
        axis.text = element_text(size = 20),    # 设置坐标轴刻度字体大小
        legend.title = element_text(size = 28), # 设置图例标题字体大小
        legend.text = element_text(size = 24),   # 设置图例文本字体大小
        legend.position = c(0.8, 0.8),          # 将图例移到图内，右上角 (80% x, 80% y)
  )
p4b
# dev.off()



####################### 5.卡方分布 ##################
# 创建数据
library(ggplot2)
# 创建数据
x <- seq(0, 15, by = 0.01)
data_chisq <- data.frame(
  x = rep(x, 3),  # 重复 x 值三次用于绘制不同的自由度
  y = c(dchisq(x, df = 3), dchisq(x, df = 5), dchisq(x, df = 7)),  # 计算三条卡方分布的 y 值
  df = factor(rep(c(3, 5, 7), each = length(x)))  # 用于区分不同自由度的曲线
)

# 绘制卡方分布图
# pdf("4.4卡方分布.pdf",width = 8, height = 4, family = "GB1")
p5a <- ggplot(data_chisq, aes(x = x, y = y, linetype = df, group = df)) +
  geom_line(size = 1) +  # 绘制曲线
  theme_classic() +
  labs(x = "x", y = "概率密度", linetype = "d") +
  ggtitle("卡方分布") + 
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) + # 使用不同线型区分
  coord_cartesian(ylim = c(0, 0.25)) +   # 设置y轴范围
  scale_y_continuous(expand = c(0, 0)) +
  theme(plot.title = element_text(size = 24),   # 设置标题字体大小
        axis.title = element_text(size = 20),   # 设置坐标轴标签字体大小
        axis.text = element_text(size = 20),    # 设置坐标轴刻度字体大小
        legend.title = element_text(size = 28), # 设置图例标题字体大小
        legend.text = element_text(size = 24),  # 设置图例文本字体大小
        legend.position = c(0.8, 0.8),          # 将图例移到图内，右上角 (80% x, 80% y)
  )
p5a
# dev.off()

a <- 0.95
x_a <- qchisq(a, df = 5)
x <- seq(0, 15, by = 0.01)
data <- data.frame(x = x, y = dchisq(x, df = 5), df = factor(rep(5, each = length(x))))

# pdf("4.4卡方分布.pdf",width = 8, height = 4, family = "GB1")
p5b <- ggplot(data, aes(x = x, y = y)) +
  geom_line(size = 1) +  # 绘制曲线
  theme_classic() + 
  labs(x = "x", y = "概率密度") + 
  ggtitle("卡方分布") + 
  geom_area(data = subset(data, x >= x_a), aes(x = x, y = y), fill = "black", alpha = 0.3) +
  annotate("segment", x = x_a, xend = x_a, y = 0, yend = dchisq(x_a, df = 5),
           color = "red", size = 1) + # 上alpha分位数线
  annotate("segment", x = x_a + 1, xend = x_a + 2, y = dchisq(x_a + 1, df = 5) - 0.01, yend = 0.05,  # 添加指向阴影部分的箭头
           color = "black", arrow = arrow(angle = 15, length = unit(0.2, "inches"), type = "closed")) +
  annotate("text",
           x = x_a + 2.2, y = 0.06,   # 设置文字的位置，略偏离箭头终点
           label = expression(alpha),    # 使用数学符号 \(\alpha\)
           size = 10, color = "black") +
  coord_cartesian(ylim = c(0, 0.2)) +   # 设置y轴范围
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(
    breaks = c(seq(0, 15, by = 5), x_a),
    labels = parse(text = paste0(sapply(c(seq(0, 15, by = 5), x_a), function(val) {
      if(abs(val - x_a) < 0.0001) {
        "chi[alpha]^2"  # 修改为卡方分布的表达式
      } else {
        val
      }
    })))
  ) +
  theme(plot.title = element_text(size = 24),   # 设置标题字体大小
        axis.title = element_text(size = 20),   # 设置坐标轴标签字体大小
        axis.text = element_text(size = 20),    # 设置坐标轴刻度字体大小
        legend.title = element_text(size = 28), # 设置图例标题字体大小
        legend.text = element_text(size = 24),  # 设置图例文本字体大小
  )
p5b



################### 6. F分布 #######################
# 创建数据
x <- seq(0, 10, by = 0.01)
params <- data.frame(df1 = c(5, 3, 5), df2 = c(20, 5, 10))  # 指定自由度组合
# 构建数据
data <- do.call(rbind, lapply(1:nrow(params), function(i) {
  df1 <- params$df1[i]
  df2 <- params$df2[i]
  y <- df(x, df1 = df1, df2 = df2)  # 计算 F 分布的概率密度
  data.frame(x = x, y = y, df1 = df1, df2 = df2)
}))
# 将 df1 和 df2 组合成因子，用于区分曲线
data$group <- factor(paste("df1=", data$df1, ", df2=", data$df2))

# 绘制F分布图
# pdf("4.6F分布.pdf",width = 8, height = 4, family = "GB1")
p6a <- ggplot(data, aes(x = x, y = y, linetype = group, group = group)) +
  geom_line(color = "black") +
  theme_classic() +
  labs(x = "x", y = "概率密度", linetype = "df1, df2") + # 使用不同线型区分
  ggtitle("F分布") + 
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) + # 使用不同线型区分
  coord_cartesian(ylim = c(0, 0.75)) +   # 设置y轴范围
  scale_y_continuous(expand = c(0, 0)) +
  theme(plot.title = element_text(size = 24),   # 设置标题字体大小
        axis.title = element_text(size = 20),   # 设置坐标轴标签字体大小
        axis.text = element_text(size = 20),    # 设置坐标轴刻度字体大小
        legend.title = element_text(size = 28), # 设置图例标题字体大小
        legend.text = element_text(size = 24),  # 设置图例文本字体大小
        legend.position = c(0.8, 0.8)           # 将图例移到图内，右上角 (80% x, 80% y)
  )
p6a
# dev.off()



a <- 0.95
x_a <- qf(a, df1 = 5, df2 = 20)
x <- seq(0, 5, by = 0.01)
# 构建数据
data <- data.frame(x = x, y = df(x, df1 = 5, df2 = 20), df1 = 5, df2 = 20)

# 绘制F分布图
# pdf("4.6F分布.pdf",width = 8, height = 4, family = "GB1")
p6b <- ggplot(data, aes(x = x, y = y)) + 
  geom_line(color = "black") +
  theme_classic() +
  labs(x = "x", y = "概率密度") + 
  ggtitle("F分布") + 
  geom_area(data = subset(data, x >= x_a), aes(x = x, y = y), fill = "black", alpha = 0.3) +
  annotate("segment", x = x_a, xend = x_a, y = 0, yend = df(x_a, df1 = 5, df2 = 20),
           color = "red", size = 1) + # 上alpha分位数线
  annotate("segment", x = 3, xend = 3.3, y = df(3, df1 = 5, df2 = 20) - 0.005, yend = 0.1,  # 添加指向阴影部分的箭头
           color = "black", arrow = arrow(angle = 15, length = unit(0.2, "inches"), type = "closed")) +
  annotate("text",
           x = 3.45, y = 0.11,   # 设置文字的位置，略偏离箭头终点
           label = expression(alpha),    # 使用数学符号 \(\alpha\)
           size = 10, color = "black") +
  coord_cartesian(ylim = c(0, 0.75)) +   # 设置y轴范围
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(
    breaks = c(seq(0, 5, by = 1), x_a),
    labels = parse(text = paste0(sapply(c(seq(0, 5, by = 1), x_a), function(val) {
      if(abs(val - x_a) < 0.0001) {
        "F[alpha]"  # 修改为卡方分布的表达式
      } else {
        val
      }
    })))
  ) + 
  theme(plot.title = element_text(size = 24),   # 设置标题字体大小
        axis.title = element_text(size = 20),   # 设置坐标轴标签字体大小
        axis.text = element_text(size = 20),    # 设置坐标轴刻度字体大小
        legend.title = element_text(size = 28), # 设置图例标题字体大小
        legend.text = element_text(size = 24),  # 设置图例文本字体大小
  )
p6b




############################### 7. weibull 分布 #############################
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
# pdf("4.3weibull分布.pdf",width = 8, height = 4, family = "GB1")
p7a <- ggplot(data, aes(x = x, y = y, linetype = shape, group = shape)) +
  geom_line(size = 1) +  # 绘制曲线
  theme_classic() +
  labs(x = "x", y = "概率密度", linetype = "k") +  # 添加图例标题
  ggtitle("Weibull分布") +
  scale_linetype_manual(values = c("solid", "dashed", "dotted"))  + # 自定义线型类型
  coord_cartesian(ylim = c(0, 1.25)) +   # 设置y轴范围
  scale_y_continuous(expand = c(0, 0)) +
  theme(plot.title = element_text(size = 24),   # 设置标题字体大小
        axis.title = element_text(size = 20),   # 设置坐标轴标签字体大小
        axis.text = element_text(size = 20),    # 设置坐标轴刻度字体大小
        legend.title = element_text(size = 28), # 设置图例标题字体大小
        legend.text = element_text(size = 24),  # 设置图例文本字体大小
        legend.position = c(0.8, 0.8),          # 将图例移到图内，右上角 (80% x, 80% y)
  )
p7a
# dev.off()

a <- 0.95
x_a <- qweibull(0.95, shape = 2, scale = 1)
x <- seq(0, 2.5, by = 0.01)
data <- data.frame(x = x, y = dweibull(x, shape = 2, scale = 1), shape = factor(rep(2, each = length(x))))
p7b <- ggplot(data, aes(x = x, y = y)) +
  geom_line(size = 1) +  # 绘制曲线
  theme_classic() +
  labs(x = "x", y = "概率密度") + 
  ggtitle("Weibull分布") + 
  geom_area(data = subset(data, x >= x_a - 0.01), aes(x = x, y = y), fill = "black", alpha = 0.3) + # 这里 - 0.01是因为图像不知道什么原因没有对上
  annotate("segment", x = x_a, xend = x_a, y = 0, yend = dweibull(x_a, shape = 2, scale = 1),
           color = "red", size = 1) + # 上alpha分位数线
  annotate("segment", x = x_a + 0.05, xend = x_a + 0.2, y = dweibull(x_a, shape = 2, scale = 1) - 0.07, yend = 0.2,  # 添加指向阴影部分的箭头
           color = "black", arrow = arrow(angle = 15, length = unit(0.2, "inches"), type = "closed")) +
  annotate("text",
           x = x_a + 0.23, y = 0.2,   # 设置文字的位置，略偏离箭头终点
           label = expression(alpha),    # 使用数学符号 \(\alpha\)
           size = 10, color = "black") +
  coord_cartesian(ylim = c(0, 1)) +   # 设置y轴范围
  scale_y_continuous(expand = c(0, 0)) + 
  scale_x_continuous(
    breaks = c(seq(0, 2.5, by = 0.5), x_a),
    labels = parse(text = paste0(sapply(c(seq(0, 2.5, by = 0.5), x_a), function(val) {
      if(abs(val - x_a) < 0.0001) {
        "W[alpha]"  # 修改为卡方分布的表达式
      } else {
        val
      }
    })))
  ) +
  theme(plot.title = element_text(size = 24),   # 设置标题字体大小
        axis.title = element_text(size = 20),   # 设置坐标轴标签字体大小
        axis.text = element_text(size = 20),    # 设置坐标轴刻度字体大小
        legend.title = element_text(size = 28), # 设置图例标题字体大小
        legend.text = element_text(size = 24),  # 设置图例文本字体大小
  )
p7b




pdf("分布图(7).pdf", width = 16, height = 22)
grid.arrange(p1a, p1b, p2a, p2b, p3a, p3b, p4a, p4b, p5a, p5b, p6a, p6b, p7a, p7b, ncol = 2, nrow = 7)
dev.off()


########################################################################################