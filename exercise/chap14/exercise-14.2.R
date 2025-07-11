library(e1071)
library(caret)
data(Loblolly)

df <- Loblolly
df$Seed <- as.factor(df$Seed)

set.seed(123)
ctrl <- trainControl(method = "cv", number = 10)
grid <- expand.grid(sigma = c(0.01, 0.05), C = c(1, 10, 100))

model_svm <- train(
  height ~ age + Seed, data = df,
  method = "svmRadial",
  trControl = ctrl,
  tuneGrid = grid
)

print(model_svm)
plot(model_svm)
