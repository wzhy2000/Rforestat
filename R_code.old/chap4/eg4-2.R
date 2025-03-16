data <- c(12.6, 13.4, 12.8, 13.2)

sample_mean <- mean(data)
sample_sd <- sd(data)
sample_size <- 4
confidence_level <- 0.95
df <- sample_size - 1

t_value <- qt((1 + confidence_level) / 2, df)
margin_of_error <- (sample_sd * t_value) / sqrt(sample_size)
lower_bound <- sample_mean - margin_of_error
upper_bound <- sample_mean + margin_of_error

cat("置信区间为: [", lower_bound, ", ", upper_bound, "]\n")
