library(e1071)
train.data <- read.csv("training.csv", sep = ",")
set.seed(123)
x.train <- train.data[, 2:(ncol(train.data))]
y.train <- train.data[, 1]
test.data <- read.csv("testing.csv", sep = ",")
x.test <- test.data[, 2:(ncol(test.data))]
y.test <- test.data[, 1]

model <- naiveBayes(x.train, as.factor(y.train))

y.pred <- predict(model, x.test)
accuracy <- mean(y.pred == y.test)
cat("Model Accuracy:", round(accuracy, 2), "\n")

library(caret)
cm <- confusionMatrix(as.factor(y.pred), as.factor(y.test))
print(cm)


