n <- 10; a <- 10; b <- 2
x <- sample(1:10, size=n, replace=TRUE)
eps <- rnorm(n, 0, 0.5)
y <- a + b * x + eps

lm(y ~ x)
summary(lm(y ~ x))
summary(lm(y ~ x))$coefficients
c(summary(lm(y ~ x))$coefficients[,1:2])

reg.sim <- function(
      a=10, b=2, sigma=0.5, 
      n=10, B=1000){
      set.seed(1)
      resm <- replicate(B, {
      x <- sample(1:10, size=n, replace=TRUE)
      eps <- rnorm(n, 0, 0.5)
      y <- a + b * x + eps
      c(summary(lm(y ~ x))$coefficients[,1:2])})
      resm <- t(resm)
      colnames(resm) <- c('a', 'b', 'SE.a', 'SE.b')
      cat(B, '次模拟的平均值:\n')
      print( apply(resm, 2, mean) )
      cat(B, '次模拟的标准差:\n')
      print( apply(resm, 2, sd) )
}
set.seed(1)
reg.sim()
