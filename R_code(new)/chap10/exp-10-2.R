library("forestat")
library(nlme)


# 其中AGB是Stem，Branch，Foliage和Fruit之和
data(picea)
picea$AGB <- picea$STEM + picea$BRANCH + picea$FOLIAGE + picea$FRUIT

set.seed(123)
train.index <- sample(
  seq_len(nrow(picea)), size = floor(0.7 * nrow(picea))
)
train.data <- picea[train.index, ]
test.data <- picea[-train.index, ]

dim(train.data)
dim(test.data)


# 幂函数（异速生长模型）
model.allometry <- nls(AGB ~ a * D0^b + c * H0^d, start = c(a = 1, b = 1, c = 1, d = 1), data = train.data)
summary(model.allometry)
cat(AIC(model.allometry), BIC(model.allometry))
FittingEvaluationIndex(predict(model.allometry, newdata = train.data), train.data$AGB)
FittingEvaluationIndex(predict(model.allometry, newdata = test.data), test.data$AGB)



# 指数函数
model.exp <- nls(AGB ~ a * exp(b * D0 + c * H0), start = c(a = 1, b = 0.1, c = 0.1), data = train.data)
summary(model.exp)
cat(AIC(model.exp), BIC(model.exp))
FittingEvaluationIndex(predict(model.exp, newdata = train.data), train.data$AGB)
FittingEvaluationIndex(predict(model.exp, newdata = test.data), test.data$AGB)

# 指数饱和模型：胸径采用饱和响应，树高采用线性项
model.saturation <- nls(
  AGB ~ a * (1 - exp(-b * D0^c)) + d * H0,
  start = list(a = 1000, b = 3e-6, c = 3, d = 5),
  algorithm = "port", lower = c(a = 0, b = 0, c = 0, d = 0),
  control = nls.control(maxiter = 1000),
  data = train.data
)
summary(model.saturation)
cat(AIC(model.saturation), BIC(model.saturation))
FittingEvaluationIndex(predict(model.saturation, newdata = train.data), train.data$AGB)
FittingEvaluationIndex(predict(model.saturation, newdata = test.data), test.data$AGB)

