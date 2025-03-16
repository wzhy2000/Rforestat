library(dplyr)
library(forestat)
library(glmnet)
# install.packages("D:\forestat_1.1.0.tar.gz", repos = NULL, type = "source")
# 读取数据
data("picea")

picea <- picea %>% select(-X, -Y, -Z, -`LIDAR-X`, -`LIDAR-Y`, -PLOT1, -OBS, -PLOT)

# picea = select(crassifolia, `LIDAR-X`, LH, LCW, CPA, Z, Plot1, Obs, H0, HCB0, CW, Stem, Branch, Foliage, Fruit)
# picea = select(crassifolia, LH, LHCB, LCW1, LCW2, LCW, CPA, D0, H0, HCB0, CW01, CW02, CW, Stem, Branch, Foliage, Fruit)


X <- model.matrix(LH ~ ., picea)[, -1]
y <- picea$LH

# 划分数据集
set.seed(123)
train <- sample(1:nrow(X) / 2)
test <- (-train)

# install.packages("glmnet")

# 导入glmnet前首先要导入依赖包Matrix
# library(Matrix)


##############
# 岭回归
##############
model.ridge <- glmnet(X[train, ], y[train], alpha = 0)

dim(coef(model.ridge))

set.seed(123)
cv.out <- cv.glmnet(X[train, ], y[train], alpha = 0)
plot(cv.out)


bestlambda <- cv.out$lambda.min
bestlambda

y.ridge.pred <- predict(model.ridge, s = bestlambda, newx = X[test, ])
mean((y.ridge.pred  - y[test])^2)


FittingEvaluationIndex(y.ridge.pred, y[test])


model.ridge.conf <- glmnet(X, y, alpha = 0)
y.ridge.conf <- predict(model.ridge.conf, type = "coefficient", s = bestlambda)[1:16, ]
round(y.ridge.conf, 4)

y.ridge.pred.nonoptimal <- predict(model.ridge, s = 10, newx = X[test, ])
mean((y.ridge.pred.nonoptimal - y[test])^2)

FittingEvaluationIndex(y.ridge.pred.nonoptimal, y[test])


##############
# lasso
##############
model.lasso <- glmnet(X[train, ], y[train], alpha = 1)
set.seed(123)
cv.out <- cv.glmnet(X[train, ], y[train], alpha = 1)
plot(cv.out)


bestlambda <- cv.out$lambda.min
bestlambda

# 预测并计算误差
y.lasso.pred <- predict(model.lasso, s = bestlambda, newx = X[test, ])
mean((y.lasso.pred - y[test])^2)
FittingEvaluationIndex(y.lasso.pred, y[test])


model.lasso.conf <- glmnet(X, y, alpha = 1)
y.lasso.conf <- predict(model.lasso.conf, type = "coefficient", s = bestlambda)[1:16, ]
round(y.lasso.conf, 4)
y.lasso.conf[y.lasso.conf == 0]


# 模型比较
anova(model.ridge, model.lasso)




