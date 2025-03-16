library(forestat)
data("birch")
plot2 <- birch[which(birch$PLOT == 2), ]
plot.mu <- 11
plot.var <- 16
t.test(plot2$D, mu = plot.mu)
library(EnvStats)
varTest(plot2$D, sigma.squared = plot.var)
