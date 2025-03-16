sample.mean <- 13; sample.sd <- 0.3; sample.size <- 4
conf.level <- 0.95
z.value <- qnorm((1 + conf.level) / 2)
margin.error <- (sample.sd * z.value) / sqrt(sample.size)
lower.bound <- sample.mean - margin.error
upper.bound <- sample.mean + margin.error
cat("置信区间为: [", lower.bound, ",", upper.bound, "]\n")
