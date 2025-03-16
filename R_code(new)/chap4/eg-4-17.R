set.seed(123)
n <- 100
dbh <- runif(n, min = 5, max = 50)
hdom <- 30

beta1 <- 0.5
beta2 <- 1.5
height <- 1.3 + (beta1 * hdom * dbh) / (beta2 + dbh)