data <- c(30.1, 30.2, 29.8, 30.3, 30.2, 29.6)
sample.size <- length(data)
sample.mean <- 30
alpha <- 0.05
df <- sample.size
sum.diff2 <- sum((data - sample.mean)^2)
chi2.lower <- qchisq(alpha / 2, df)
chi2.upper <- qchisq(1 - alpha / 2, df)
var.lower <- sum.diff2 / chi2.upper
var.upper <- sum.diff2 / chi2.lower
cat("置信区间为: [", var.lower, ", ", var.upper, "]\n")