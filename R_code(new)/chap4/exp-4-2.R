est_pi <- function(N) {
  x1 <- runif(N, 0, 1)
  x2 <- runif(N, 0, 1)
  inside <- x1^2 + x2^2 <= 1
  hat_pi <- 4 * mean(inside)
  se <- sqrt(hat_pi * (4 - hat_pi) / N)
  list(N = N, pi = hat_pi, se = se)
}

set.seed(123)
pi1 <- est_pi(1E4)
cat("N = ", pi1$N, " pi估计值 =", pi1$pi, " SE =", pi1$se, "\n")
pi2 <- est_pi(1E6)
cat("N = ", pi2$N, " pi估计值 =", pi2$pi, " SE =", pi2$se, "\n")