library(systemfit)
options(digits = 5)

set.seed(123)
n <- 100
data <- data.frame(
  y1 = rnorm(n),
  x1 = rnorm(n),
  y2 = rnorm(n),
  x2 = rnorm(n)
)

eq1 <- y1 ~ x1 + 1
eq2 <- y2 ~ x2 + 1

eqs <- list(eq1, eq2)

system <- systemfit(eqs, data = data)

