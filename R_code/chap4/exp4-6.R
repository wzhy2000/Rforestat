est_pi <- function(N){
    set.seed(101)
    x1 <- runif(N, 0, 1)
    x2 <- runif(N, 0, 1)
    y <- as.numeric(x1^2 + x2^2 <= 1)
    hat_pi <- 4*mean(y)
    se <- 4 * sd(y) / sqrt(N)
    cat("N = ", N, " pi估计值 =", hat_pi, " SE =", se, "\n")
    invisible(list(N=N, hat_pi = hat_pi, SE = se))}

est_pi(1E4)
est_pi(1E6)