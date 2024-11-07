library(mvtnorm)
library("gamreg")
n <- 30                     
p <- 10                     
epsilon <- 0.1              

beta0 <- 0.0                
beta <- c(numeric(p))       
beta[1] <- 1
beta[2] <- 2
beta[3] <- 3
beta[4] <- 4

Sigma <- 0.2^t(sapply(1:p, function(i, j) abs(i-j), 1:p))
X <- rmvnorm(n, sigma=Sigma)
e <- rnorm(n)               
i <- 1:ceiling(epsilon*n)   
e[i] <- e[i] + 20            
Y <- beta0*(numeric(n)+1) + X%*%beta

res <- cv.gam(X, Y, nlambda = 5, nlambda.LTS = 20, init.mode = "sLTS")

print(res)
