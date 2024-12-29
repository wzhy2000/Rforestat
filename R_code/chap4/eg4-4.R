sample_variance <- 0.0009
sample_size <- 4
alpha <- 0.05

df <- sample_size - 1

chi_square_lower <- qchisq(alpha / 2, df)
chi_square_upper <- qchisq(1 - alpha / 2, df)

variance_lower <- (df * sample_variance) / chi_square_upper
variance_upper <- (df * sample_variance) / chi_square_lower

std_dev_lower <- sqrt(variance_lower)
std_dev_upper <- sqrt(variance_upper)

cat("方差的置信区间为: [", variance_lower, ", ", variance_upper, "]\n")
cat("标准差的置信区间为: [", std_dev_lower, ", ", std_dev_upper, "]\n")