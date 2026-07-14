set.seed(123)
n <- 100
x <- runif(n, 0, 10)
a_true <- 2
b_true <- 0.3
y <- a_true * exp(b_true * x) + rnorm(n, sd = 1)
df <- data.frame(x = x, y = y)
library(boot)

fit0 <- nls(
  y ~ a * exp(b * x),
  data = df,
  start = list(a = 1, b = 0.1)
)

coef0 <- coef(fit0)
sigma0 <- sd(residuals(fit0))


stat_fun <- function(data, indices) {
  fit <- try(nls(y ~ a * exp(b * x), data = data[indices, ],
                 start = list(a = 1, b = 0.1)), silent = TRUE)
  if (inherits(fit, "try-error")) return(c(a = NA, b = NA))
  coef(fit)
}

ran_gen <- function(data, mle) {
  mu <- mle["a"] * exp(mle["b"] * data$x)
  transform(data, y = mu + rnorm(nrow(data), 0, mle["sigma"]))
}

mle <- c(coef(fit0), sigma = sigma0)
set.seed(123)
boot_par <- boot(df, statistic = stat_fun, R = 2000,
                 sim = "parametric", ran.gen = ran_gen, mle = mle)
ok <- complete.cases(boot_par$t)
cat("成功次数:", sum(ok), "失败次数:", sum(!ok), "\n")


bias_a <- mean(boot_par$t[ok, 1]) - coef0[["a"]]
se_a <- sd(boot_par$t[ok, 1])
bias_b <- mean(boot_par$t[ok, 2]) - coef0[["b"]]
se_b <- sd(boot_par$t[ok, 2])

cat("a: bias =", bias_a, "SE =", se_a, "\n")
cat("b: bias =", bias_b, "SE =", se_b, "\n")

