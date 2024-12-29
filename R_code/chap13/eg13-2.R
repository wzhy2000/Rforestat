library(e1071)
model <- svm(Species ~ ., data = iris, kernel = "linear")
summary(model)
