library(forestat)
data("birch")
attach(birch)
set.seed(123)

sample <- sample(birch$D, 50)
mu <- 11

t.test(sample, mu = mu)
