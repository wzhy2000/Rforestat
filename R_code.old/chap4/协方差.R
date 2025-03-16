data <- read.csv("data-{eg4-10}.csv")
attach(data)
fit1 <- aov(Height0 ~ plot)  # 方差分析
summary(fit1) 


table(plot) # 计数

# 将plot声明为因子，视为协方差分析中的处理效应
# tree.age为连续变量，视为协变量
plot <- as.factor(plot)
fit2 <-aov(Height0 ~ plot + DBH) # 协方差分析
summary(fit2)
