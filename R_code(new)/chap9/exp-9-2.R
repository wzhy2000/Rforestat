library("forestat")
library(nlme)


# 其中AGB是Stem，Branch，Foliage和Fruit之和
data(picea)
picea$AGB <- picea$STEM + picea$BRANCH + picea$FOLIAGE + picea$FRUIT
attach(picea)

set.seed(123)
datapartde <-sample(2, nrow(picea), replace = TRUE, prob = c(0.7,0.3))
train.data <- picea[datapartde == 1, ]
test.data <- picea[datapartde == 2, ]

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

# Richard
model.richard <- nls(AGB ~ a * (1 - exp(-b * D0 + c * H0)), start = c(a = 1, b = -0.1, c = 0.1), 
            data = train.data)
summary(model.richard)
cat(AIC(model.richard), BIC(model.richard))
FittingEvaluationIndex(predict(model.richard, newdata = train.data), train.data$AGB)
FittingEvaluationIndex(predict(model.richard, newdata = test.data), test.data$AGB)

