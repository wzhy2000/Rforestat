# 生成数据
set.seed(123)
x <- seq(0, 100, 1)
y <- runif(1, 0, 20) * exp(runif(1, 0.005, 0.075) * x) + runif(101, 0, 5)
data = data.frame(x, y)

# selfStart函数
myinitial <- function(mCall, LHS, data, ...) {
  xy <- sortedXyData(mCall[["x"]], LHS, data)
  x <- xy[["x"]]
  y <- xy[["y"]]
  
  if (any(y <= 0)) stop("All values of 'y' must be positive to take the logarithm.")
  y <- log(y)
  
  aux <- coef(lm(y ~ x))
  a <- exp(aux[1])  # 截距转换回原比例
  b <- aux[2]       # 斜率
  
  value <- c(a = a, b = b)
  return(value)
}

myselfStart <- selfStart(
  ~ a * exp(b * x), 
  initial = myinitial, 
  parameters = c("a", "b")
)

# 获得初始值
start <- getInitial(y ~ myselfStart(x, a, b), data = data)
start
start <- as.list(start)

# 模型拟合
model <- nls(y ~ a*exp(b*x), data = data, start = c(a = start$a,b = start$b.x))
summary(model)

# 结果可视化
data$fitted_value <- predict(model)
library(ggplot2)
pdf("图8.2.pdf", width = 8, height = 6, family = "GB1")
ggplot(data, aes(x = x, y = y)) + 
  geom_point(color = "blue", size = 2, alpha = 0.7) +  
  geom_line(aes(x = x, y = fitted_value), color = "red", size = 1) +  
  labs(
    title = "模型拟合结果",
    x = "x (Predictor)",
    y = "y (Response)"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 24, face = "bold"),  # 标题字体大小
    axis.title.x = element_text(size = 24),               # x 轴标签字体大小
    axis.title.y = element_text(size = 24),               # y 轴标签字体大小
    axis.text.x = element_text(size = 24),                # x 轴刻度字体大小
    axis.text.y = element_text(size = 24),                # y 轴刻度字体大小
    legend.title = element_text(size = 24),               # 图例标题字体大小
    legend.text = element_text(size = 24)                 # 图例内容字体大小
  )

dev.off()

data$residuals <- residuals(model)
pdf("图8.3.pdf", width = 8, height = 6, family = "GB1")
ggplot(data, aes(x = x, y = residuals)) +
  geom_point(color = "blue", size = 2, alpha = 0.7) +  # 残差点
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 1) +  # 零线
  labs(
    title = "残差图",
    x = "拟合值",
    y = "残差"
  ) +
  theme_classic()+
  theme(
    plot.title = element_text(size = 24, face = "bold"),  # 标题字体大小
    axis.title.x = element_text(size = 24),               # x 轴标签字体大小
    axis.title.y = element_text(size = 24),               # y 轴标签字体大小
    axis.text.x = element_text(size = 24),                # x 轴刻度字体大小
    axis.text.y = element_text(size = 24),                # y 轴刻度字体大小
    legend.title = element_text(size = 24),               # 图例标题字体大小
    legend.text = element_text(size = 24)                 # 图例内容字体大小
  )
dev.off()