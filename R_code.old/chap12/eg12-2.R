# 加载数据
library(BLR)
data(wheat)
y = Y[, 1]
set.seed(123)

# 划分数据集
whichNa <- sample(1:length(y), size=150, replace=FALSE)
y[whichNa] <- NA

blr<- BLR(
  y = y,
  XL = X,
  prior = list(
    varE = list(df=3, S=0.25),
    varU = list(df=3, S=0.63),
    lambda = list(shape=0.5, rate=0.0001, type='random', value=30)),
  nIter = 10000,
  burnIn = 1000,
  thin = 1
)

y = Y[, 1]
# 模型评估
MSE.tst <- mean((blr$yHat[whichNa] - y[whichNa])^2)
print(paste("Test Set Mean Squared Error: ", MSE.tst))

MSE.trn <- mean((blr$yHat[-whichNa] - y[-whichNa])^2)
print(paste("Training Set Mean Squared Error: ", MSE.trn))

COR.tst <- cor(blr$yHat[whichNa], y[whichNa])
print(paste("Test Set Correlation: ", COR.tst))

COR.trn <- cor(blr$yHat[-whichNa], y[-whichNa])
print(paste("Training Set Correlation: ", COR.trn))

