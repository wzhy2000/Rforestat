library(vegan)
library(pwr)
data(mite)
species1_abundance <- mite[,1]
mu_test <- 5
t_res <- t.test(species1_abundance, mu = mu_test, conf.level = 0.95)
print(t_res)

n <- length(species1_abundance)
d <- abs(mean(species1_abundance) - mu_test) / sd(species1_abundance)
alpha <- 0.05
power_res <- pwr.t.test(n = n, d = d, sig.level = alpha,
                        type = "one.sample", alternative = "two.sided")
cat("当前检验功效(power):", power_res$power, "\n")
sample_size_needed <- pwr.t.test(d = d, sig.level = alpha, power = 0.85,
                                 type = "one.sample", alternative = "two.sided")
cat("达到0.85检验功效所需样本量:", ceiling(sample_size_needed$n), "\n")

