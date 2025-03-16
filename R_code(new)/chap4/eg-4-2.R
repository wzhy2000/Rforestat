data <- c(12.6, 13.4, 12.8, 13.2)
sample.mean <- mean(data)
sample.sd <- sd(data)
sample.size <- length(data)
conf.level <- 0.95
df <- sample.size - 1
t.value <- qt((1 + conf.level) / 2, df)
margin.error <- (sample.sd * t.value) / sqrt(sample.size)
lower.bound <- sample.mean - margin.error
upper.bound <- sample.mean + margin.error
cat("置信区间为: [", lower.bound, ", ", upper.bound, "]\n")