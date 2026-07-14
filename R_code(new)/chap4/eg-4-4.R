data <- c(12.6, 13.4, 12.8, 13.2)
sample.var <- var(data)
sample.size <- length(data)
alpha <- 0.05
df <- sample.size - 1
chi2.lower <- qchisq(alpha / 2, df)
chi2.upper <- qchisq(1 - alpha / 2, df)
var.lower <- (df * sample.var) / chi2.upper
var.upper <- (df * sample.var) / chi2.lower
sd.lower <- sqrt(var.lower)
sd.upper <- sqrt(var.upper)
cat("方差的置信区间为: [", var.lower, ", ", var.upper, "]\n")
cat("标准差的置信区间为: [", sd.lower, ", ", sd.upper, "]\n")