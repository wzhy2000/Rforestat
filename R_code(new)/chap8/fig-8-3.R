set.seed(6)
x = seq(0, 5, length.out = 100)
data <- data.frame(
  x = seq(0, 5, length.out = 100),
  y = sin(x) + 0.5 + rnorm(100, sd = 0.5)
)

loess_model <- loess(y ~ x, data = data, span = 0.5)

x_value <- 2
y_fit <- predict(loess_model, newdata = data.frame(x = x_value))
slope <- predict(loess_model, newdata = data.frame(x = x_value), se = TRUE)$se.fit
pdf("局部回归示例图.pdf", width = 10, height = 6, family = "GB1")
par(mar = c(4, 5.2, 2, 2), mgp = c(3, 1, 0))
plot(data$x, data$y, main = "", 
     xlab = "X轴", ylab = "Y轴", pch = 19, col = "gray", xlim = c(0, 5), ylim = c(-2, 2), cex.lab = 2, cex.axis = 2)
points(data$x[data$x >= 1 & data$x <= 3], data$y[data$x >= 1 & data$x <= 3], 
       pch = 19, col = "black")

lines(data$x, predict(loess_model), col = "gray50", lwd = 2, lty = 2)

points(x_value, y_fit, pch = 17, col = "red", cex = 1.5)

slope <- predict(loess_model, newdata = data.frame(x = x_value), se = TRUE)$se.fit
intercept <- y_fit - slope * x_value
x_line <- seq(1, 3, length.out = 2) # x的取值范围从1到3
y_line <- 2.4-0.5* x_line
lines(x_line, y_line, col = "gray30", lwd = 2, lty = 3)

lines(seq(0, 10, length.out = 1000), sin(seq(0, 10, length.out = 1000))+0.5, col = "black", lwd = 2)

curve(dnorm(x, mean = 2, sd = 0.3), add = TRUE, col = "gray", lwd = 2, from = 1, to = 3)

x_seq <- seq(1, 3, length.out = 1000)
y_seq <- dnorm(x_seq, mean = 2, sd = 0.3)

polygon(c(x_seq, rev(x_seq)), c(y_seq, rep(0, length(y_seq))), col = rgb(0.5, 0.5, 0.5, 0.5), border = NA)
dev.off()
