library(forestat)
library(ggplot2)
data("picea")
data <- picea$LH
hist(data, main = "原始数据直方图", xlab = "树高", col = "skyblue", breaks = 20)
qqnorm(data, main = "原始数据 Q-Q图")
qqline(data, col = "red")
shapiro.test(data)
log_data <- log(data + 1)
hist(log_data, main = "对数变换后直方图", xlab = "log(树高+1)", col = "lightgreen", breaks = 20)
qqnorm(log_data, main = "log(树高+1) Q-Q图")
qqline(log_data, col = "red")
shapiro.test(log_data)

