library(vegan)
library(caret)
data(mite)
data(mite.env)

mite.env$Shannon <- diversity(mite, index = "shannon")
df <- mite.env[, c("Shannon", "SubsDens", "WatrCont")]

set.seed(123)
ctrl <- trainControl(method = "cv", number = 10)
model_knn <- train(
  Shannon ~ ., data = df,
  method = "knn",
  tuneLength = 10,
  trControl = ctrl
)

print(model_knn)
plot(model_knn)
