set.seed(123)
n <- 100
x <- runif(n, 0, 10)
a_true <- 2
b_true <- 0.3
y <- a_true * exp(b_true * x) + rnorm(n, sd = 1)
df <- data.frame(x = x, y = y)

library(boot)
param_fa <- function(d, inds) {
  fit <- nls(y ~ a * exp(b * x), data = d, start = list(a = 1, b = 0.1))
  x_new <- runif(n, min = min(d$x), max = max(d$x))
  y_new <- predict(fit, newdata = data.frame(x = x_new)) + 
    rnorm(n, sd = sd(residuals(fit)))
  dd <- data.frame(x = x_new, y = y_new)
  fit_new <- nls(y ~ a * exp(b * x), data = dd, start = list(a = 1, b = 0.1))
  unname(coef(fit_new))
}
param_btr <- boot(df, param_fa, R = 1000)

cat("a: bias=", param_btr$t0[1] - mean(param_btr$t[, 1]), "SE=",
    sd(param_btr$t[, 1]), "\n")
cat("b: bias=", param_btr$t0[2] - mean(param_btr$t[, 2]), "SE=",
    sd(param_btr$t[, 2]), "\n")