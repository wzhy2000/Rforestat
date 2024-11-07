mean_A <- 150
sd_A <- 20
n_A <- 30
mean_B <- 140
sd_B <- 25
n_B <- 40

Z <- (mean_A - mean_B) / sqrt(sd_A^2 / n_A + sd_B^2 / n_B)

p_value <- 2 * pnorm(-abs(Z))

print(sprintf("Z-value: %f", Z))
print(sprintf("P-value: %f", p_value))