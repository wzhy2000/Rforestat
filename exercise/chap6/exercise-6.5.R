# 习题 6.5：统一划分下比较线性回归、岭回归和 LASSO

library(glmnet)

# （1）示例研究问题：用汽车重量等连续特征预测燃油经济性 mpg。
# 数据来源为 datasets::mtcars；mpg 单位为英里/加仑，wt 单位为 1000 磅。
dat <- datasets::mtcars[, c("mpg", "disp", "hp", "drat", "wt", "qsec")]
stopifnot(!anyNA(dat))
set.seed(123)
train_index <- sample(seq_len(nrow(dat)), size = floor(0.8 * nrow(dat)))
train <- dat[train_index, ]
test <- dat[-train_index, ]
cat("训练/测试样本量：", nrow(train), "/", nrow(test), "\n")

# （2）在同一训练样本上拟合简单和多重线性回归，并检查诊断。
simple_lm <- lm(mpg ~ wt, data = train)
multiple_lm <- lm(mpg ~ disp + hp + drat + wt + qsec, data = train)
print(summary(simple_lm))
print(confint(simple_lm))
print(summary(multiple_lm))
print(confint(multiple_lm))

if (!interactive()) grDevices::cairo_pdf(tempfile("exercise-6.5-", fileext = ".pdf"), width = 9, height = 6)
par(mfrow = c(2, 2))
plot(simple_lm, which = 1)
plot(simple_lm, which = 2)
plot(multiple_lm, which = 1)
plot(multiple_lm, which = 2)
if (!interactive()) dev.off()

# （3）编码、标准化和 lambda 选择均只使用训练数据。
formula_all <- mpg ~ disp + hp + drat + wt + qsec
x_train <- model.matrix(formula_all, train)[, -1, drop = FALSE]
x_test <- model.matrix(formula_all, test)[, -1, drop = FALSE]
set.seed(123)
ridge <- cv.glmnet(x_train, train$mpg, alpha = 0, nfolds = 5)
set.seed(123)
lasso <- cv.glmnet(x_train, train$mpg, alpha = 1, nfolds = 5)
print(coef(ridge, s = "lambda.min"))
print(coef(lasso, s = "lambda.min"))

# （4）四个模型在完全相同的测试样本上比较。
predictions <- list(
  simple_lm = predict(simple_lm, newdata = test),
  multiple_lm = predict(multiple_lm, newdata = test),
  ridge = as.numeric(predict(ridge, x_test, s = "lambda.min")),
  lasso = as.numeric(predict(lasso, x_test, s = "lambda.min"))
)
metrics <- function(predicted) c(
  RMSE = sqrt(mean((test$mpg - predicted)^2)),
  MAE = mean(abs(test$mpg - predicted)),
  R2 = 1 - sum((test$mpg - predicted)^2) / sum((test$mpg - mean(test$mpg))^2)
)
comparison <- do.call(rbind, lapply(predictions, metrics))
print(comparison)
cat("线性模型便于解释；正则化可提高系数稳定性，但预测优劣必须由统一测试集判断。\n")
