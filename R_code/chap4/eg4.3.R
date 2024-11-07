measurements <- c(30.1, 30.2, 29.8, 30.3, 30.2, 29.6)
sample_mean <- 30
sample_size <- length(measurements)
alpha <- 0.05

sum_squared_diff <- sum((measurements - sample_mean)^2)

df <- sample_size
chi_square_lower <- qchisq(alpha / 2, df)
chi_square_upper <- qchisq(1 - alpha / 2, df)

variance_lower <- sum_squared_diff / chi_square_upper
variance_upper <- sum_squared_diff / chi_square_lower
cat("置信区间为: [", variance_lower, ", ", variance_upper, "]\n")