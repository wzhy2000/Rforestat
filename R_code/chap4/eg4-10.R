set.seed(42)
n <- 100
dbh <- runif(n, min=5, max=50)
hdom <- 30

beta1 <- 0.5
beta2 <- 1.5
height <- 1.3 + (beta1 * hdom * dbh) / (beta2 + dbh)

plot(dbh, height, main="树高与胸径关系", xlab="胸径 (cm)", ylab="树高 (m)")
abline(lm(height ~ dbh), col="red")