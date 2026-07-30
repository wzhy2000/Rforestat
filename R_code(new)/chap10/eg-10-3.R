# 生成数据
set.seed(123)
a.org <- runif(1, 0, 20)
b.org <- runif(1, 0.005, 0.075)
x <- seq(0, 100, 1)
y <- a.org * exp(b.org * x) + rnorm(length(x), mean = 0, sd = 2.5)
sim.data <- data.frame(x = x, y = y)

# selfStart函数
myinitial <- function(mCall, LHS, data, ...) {
  xy <- sortedXyData(mCall[["x"]], LHS, data)
  x <- xy[["x"]]
  y <- xy[["y"]]
  if (any(y <= 0)) stop("All values of 'y' must be positive to take the logarithm.")
  y <- log(y)
  aux <- unname(coef(lm(y ~ x)))
  value <- c(a = exp(aux[1]), b = aux[2])
  return(value)
}
myselfStart <- selfStart(~ a * exp(b * x), initial = myinitial, parameters = c("a", "b"))


# 获得初始值
start <- getInitial(y ~ myselfStart(x, a, b), data = sim.data)
start

# 模型拟合
start <- unname(start)
names(start) <- c("a", "b")
str(start)
model.exp <- nls(y ~ a * exp(b * x), data = sim.data, start = as.list(start))
summary(model.exp)

# 结果可视化
sim.data$fitted_value <- predict(model.exp)
library(ggplot2)
pdf("图8.2.pdf", width = 8, height = 6, family = "GB1")
ggplot(sim.data, aes(x = x, y = y)) + 
  geom_point(color = "blue", size = 2, alpha = 0.7) +  
  geom_line(aes(x = x, y = fitted_value), color = "red", size = 1) +  
  labs(
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

sim.data$residuals <- residuals(model.exp)
pdf("图8.3.pdf", width = 8, height = 6, family = "GB1")
ggplot(sim.data, aes(x = fitted_value, y = residuals)) +
  geom_point(color = "blue", size = 2, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 1) +  # 零线
  labs(
    x = "Fitted value",
    y = "Residuals"
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
