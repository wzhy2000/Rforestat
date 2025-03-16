est_pi <- function(N){
  set.seed(123)
  x1 <- runif(N, 0, 1)
  x2 <- runif(N, 0, 1)
  y <- as.numeric(x1^2 + x2^2 <= 1)
  hat_pi <- 4 * mean(y)
  se <- sqrt((4 - hat_pi)*hat_pi / N)
  invisible(list(N=N, pi = hat_pi, se = se))
}
pi1 <- est_pi(1E6)
cat("N = ", pi1$N, " pi估计值 =", pi1$pi, " SE =", pi1$se, "\n")
pi2 <- est_pi(1E8)
cat("N = ", pi2$N, " pi估计值 =", pi2$pi, " SE =", pi2$se, "\n")