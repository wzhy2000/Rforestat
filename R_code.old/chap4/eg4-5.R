library(forestat)
data("birch")
attach(birch)
data <- birch[which(PLOT == 2), ]
mu <- 11
var <- 16

t.test(data$D, mu = mu)

library(EnvStats)
varTest(data$D, sigma.squared = var)
