# 习题 6.4：按树分组的岭回归与 LASSO 新树预测

library(glmnet)
data("Loblolly", package = "datasets")

# （1）按 Seed 整体划分训练/测试集；Seed 不进入预测特征。
set.seed(123)
trees <- levels(Loblolly$Seed)
train_trees <- sample(trees, size = floor(0.75 * length(trees)))
test_trees <- setdiff(trees, train_trees)
train <- droplevels(Loblolly[as.character(Loblolly$Seed) %in% train_trees, ])
test <- droplevels(Loblolly[as.character(Loblolly$Seed) %in% test_trees, ])
cat("训练树：", paste(train_trees, collapse = ", "), "\n")
cat("测试树：", paste(test_trees, collapse = ", "), "\n")
stopifnot(length(intersect(train_trees, test_trees)) == 0L)

# （2）只用 age、age²、age³；内层 foldid 也按整棵树划分。
x_train <- model.matrix(~ poly(age, 3, raw = TRUE), data = train)[, -1, drop = FALSE]
x_test <- model.matrix(~ poly(age, 3, raw = TRUE), data = test)[, -1, drop = FALSE]
set.seed(123)
inner_trees <- sample(train_trees)
tree_fold <- setNames(rep(seq_len(5), length.out = length(inner_trees)), inner_trees)
foldid <- unname(tree_fold[as.character(train$Seed)])
stopifnot(all(tapply(foldid, train$Seed, function(x) length(unique(x))) == 1L))

# （3）分别在训练树内选择 lambda。
set.seed(123)
ridge_cv <- cv.glmnet(x_train, train$height, alpha = 0, foldid = foldid)
set.seed(123)
lasso_cv <- cv.glmnet(x_train, train$height, alpha = 1, foldid = foldid)
cat("岭回归 lambda.min/lambda.1se：", ridge_cv$lambda.min, ridge_cv$lambda.1se, "\n")
cat("LASSO lambda.min/lambda.1se：", lasso_cv$lambda.min, lasso_cv$lambda.1se, "\n")
print(coef(ridge_cv, s = "lambda.min"))
print(coef(lasso_cv, s = "lambda.min"))

# （4）在从未进入训练集的新树上统一评价。
ridge_prediction <- as.numeric(predict(ridge_cv, x_test, s = "lambda.min"))
lasso_prediction <- as.numeric(predict(lasso_cv, x_test, s = "lambda.min"))
metrics <- function(observed, predicted) c(
  RMSE = sqrt(mean((observed - predicted)^2)),
  MAE = mean(abs(observed - predicted)),
  R2 = 1 - sum((observed - predicted)^2) / sum((observed - mean(observed))^2)
)
print(rbind(
  ridge = metrics(test$height, ridge_prediction),
  lasso = metrics(test$height, lasso_prediction)
))
cat("仅有 14 棵树，单次分组切分不稳定；正式研究应重复分组验证并报告区间。\n")
