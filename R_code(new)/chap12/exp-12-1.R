data <- read.csv("D:\\大连理工大学\\R语言书稿\\R_code(new)\\chap6\\data-exp-6-2.csv",
                 fileEncoding = "GBK")
data <- data[, !names(data) %in% c("X")]

set.seed(123)
index.train <- sample(1:nrow(data), 0.7 * nrow(data))  
# x.train <- data[index.train, c("D0", "BA", "BAL", "DBA", "DG", "RD", "SH")]
x.train <- data[index.train, -which(names(data) %in% c("树种名", "id"))]
y.train <- data[index.train, "树种名"]
# x.test <- data[-index.train, c("D0", "BA", "BAL", "DBA", "DG", "RD", "SH")]
x.test <- data[-index.train, -which(names(data) %in% c("树种名", "id"))]
y.test <- data[-index.train, "树种名"]

library(e1071)
model <- naiveBayes(x.train, as.factor(y.train))

y.pred <- predict(model, x.train)
accuracy <- mean(y.pred == y.train)
cat("Model Accuracy:", round(accuracy, 2), "\n")


y.pred <- predict(model, x.test)
accuracy <- mean(y.pred == y.test)
cat("Model Accuracy:", round(accuracy, 2), "\n")

library(caret)
y.test <- factor(y.test, levels = levels(as.factor(y.pred)))
cm <- confusionMatrix(as.factor(y.pred), as.factor(y.test))
print(cm)
