#Fig2-3-a
x <- 1:10
y <- x + rnorm(10)
plot(x, y, xlab = "X轴", ylab = "Y轴", pch = 19, col = "blue",
     cex.lab = 1.5, cex.axis = 1.5)
points(x, y, pch = 17, col = "red")
lines(x, y, col = "green")
text(5, 8, "示例文本", cex = 1.5) 
legend("topright", legend = c("点", "线"), col = c("red", "green"), 
       pch = c(17, NA), lty = 1, cex = 1.5)

#Fig2-3-b
categories <- c("A", "B", "C", "D", "E", "F")
values <- c(1, 2, 3, 4, 5, 6)
bp <- barplot(values, names.arg = categories, xlab = "分类", ylab = "值", 
              col = "lightblue", ylim = c(0, max(values) + 2),
              cex.lab = 1.5, cex.axis = 1.5, cex.names = 1.5)
text(bp, values + 0.3, labels = values, col = "black", cex = 1.5)

#Fig2-3-c
set.seed(123)
par(cex.lab = 1.5, cex.axis = 1.5)
categories <- c("A", "B", "C", "D", "E", "F")
values <- runif(6, min = 0, max = 10)
dotchart(values, xlab = "数值", ylab = "分类", col = "black", pch = 16)
axis(2, at = 1:6, labels = categories, cex.axis = 1.5)

#Fig2-3-d
data <- rnorm(100)
hist(data, xlab = "数值", ylab = "频率", col = "orange", xlim = c(-3, 3), 
     ylim = c(0,30), breaks = 10, cex.lab = 1.5, cex.axis = 1.5, main = "")
abline(v = mean(data), col = "red", lwd = 2)