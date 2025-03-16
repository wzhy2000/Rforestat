library(mvtnorm)
library("gamreg")
set.seed(6)
n <- 30                  # 样本量   
p <- 10                  # 预测变量数量
epsilon <- 0.1           #  离群点比例

beta0 <- 0.0                # 截距
beta <- c(numeric(p))       # 回归系数
beta[1] <- 1
beta[2] <- 2
beta[3] <- 3
beta[4] <- 4

Sigma <- 0.2^t(sapply(1:p, function(i, j) abs(i-j), 1:p)) # 构造p*p的协方差矩阵
X <- rmvnorm(n, sigma=Sigma) # 生成n个来自多元正态分布的随机样本
e <- rnorm(n)               # 随机误差项
i <- 1:ceiling(epsilon*n)    # 增加离群点
e[i] <- e[i] + 20            
Y <- beta0*(numeric(n)+1) + X%*%beta + e

res <- cv.gam(X, Y, nlambda = 5, nlambda.LTS = 20, init.mode = "sLTS")

print(res)
