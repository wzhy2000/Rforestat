
## 非参数Bootstrap

library(tidyverse)
library(boot)
set.seed(123)
n <- 100
v0 <- rnorm(n, mean = 50, sd = 10)
v1 <- 3 + 0.5 * v0 + rnorm(n, sd = 5)
d.cancer <- data.frame(v0 = v0, v1 = v1)

fa <- function(d, inds) {
      dd <- d[inds, ]
      lm1 <- lm(v1 ~ v0, data = dd)
      unname(coef(lm1))}

btr <- boot(d.cancer, fa, R = 1000)

cat("Intercept: bias=", btr$t0[1] - mean(btr$t[,1]), "SE=", sd(btr$t[,1]), "\n")
cat("v0 slope: bias=", btr$t0[2] - mean(btr$t[,2]), "SE=", sd(btr$t[,2]), "\n")



## 参数Bootstrap

set.seed(123)
param_fa <- function(d, inds) {
      lm1 <- lm(v1 ~ v0, data = d)
      v0_new <- rnorm(n, mean = mean(d$v0), sd = sd(d$v0))
      v1_new <- predict(lm1) + rnorm(n, sd = sd(residuals(lm1)))
      dd <- data.frame(v0 = v0_new, v1 = v1_new)
      unname(coef(lm(v1 ~ v0, data = dd)))}

param_btr <- boot(d.cancer, param_fa, R = 1000)

cat("Intercept: bias=", param_btr$t0[1] - mean(param_btr$t[, 1]), "SE=", sd(param_btr$t[, 1]), "\n")
cat("v0 slope: bias=", param_btr$t0[2] - mean(param_btr$t[, 2]), "SE=", sd(param_btr$t[, 2]), "\n")