library(tidyverse)
library(glmnet)
library(caret)
library(forestat)
data(Loblolly)
df <- Loblolly %>%
  mutate(Seed = as.factor(Seed))
x <- model.matrix(height ~ age + Seed, data = df)[, -1]  # 去掉Intercept列
y <- df$height
set.seed(123)
train_index <- createDataPartition(y, p = 0.8, list = FALSE)
x_train <- x[train_index, ]
y_train <- y[train_index]
x_test  <- x[-train_index, ]
y_test  <- y[-train_index]
ridge_cv <- cv.glmnet(x_train, y_train, alpha = 0)
ridge_best_lambda <- ridge_cv$lambda.min
ridge_model <- glmnet(x_train, y_train, alpha = 0, lambda = ridge_best_lambda)
ridge_pred <- predict(ridge_model, s = ridge_best_lambda, newx = x_test)
lasso_cv <- cv.glmnet(x_train, y_train, alpha = 1)
lasso_best_lambda <- lasso_cv$lambda.min
lasso_model <- glmnet(x_train, y_train, alpha = 1, lambda = lasso_best_lambda)
lasso_pred <- predict(lasso_model, s = lasso_best_lambda, newx = x_test)
ridge_eval <- FittingEvaluationIndex(as.vector(ridge_pred), y_test)
lasso_eval <- FittingEvaluationIndex(as.vector(lasso_pred), y_test)
cat("\n 岭回归评估结果：\n")
print(round(ridge_eval, 4))
cat("\n Lasso回归评估结果：\n")
print(round(lasso_eval, 4))
