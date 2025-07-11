# install.packages("MLmetrics")
library(MLmetrics)
library(e1071)
data <- read.csv("data.csv", fileEncoding = "gbk")
set.seed(42)
idx <- sample(1:nrow(data), 0.7 * nrow(data))
train <- data[idx, ]
test <- data[-idx, ]
model1 <- naiveBayes(as.factor(树种名) ~ D0 + BA + VH + CD, data = train)
pred1 <- predict(model1, test)
cat("【原始模型混淆矩阵】\n")
print(table(pred1, test$树种名))
cat("准确率 =", mean(pred1 == test$树种名), "\n")
model2 <- naiveBayes(as.factor(树种名) ~ D0 + BA + VH + CD, data = train, laplace = 1)
pred2 <- predict(model2, test)
cat("【加权模型混淆矩阵】\n")
print(table(pred2, test$树种名))
cat("准确率 =", mean(pred2 == test$树种名), "\n")
options(repos = c(CRAN = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/"))

f1_base <- F1_Score(y_pred = pred1, y_true = test$树种名, positive = "A")
f1_lap <- F1_Score(y_pred = pred2, y_true = test$树种名, positive = "A")
cat("原始模型 F1 =", f1_base, "，加权模型 F1 =", f1_lap, "\n")
wrong <- test[pred1 != test$树种名, ]
head(wrong[, c("树种名", "D0", "BA", "VH", "CD")], 5)

