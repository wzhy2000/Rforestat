sample_mean <- 13
sample_sd <- 0.37
sample_size <- 4

confidence_level <- 0.95
z_value <- qnorm((1 + confidence_level) / 2)
margin_of_error <- (sample_sd * z_value) / sqrt(sample_size)

lower_bound <- sample_mean - margin_of_error
upper_bound <- sample_mean + margin_of_error

cat("置信区间为: [",lower_bound,",",upper_bound,"]\n")
