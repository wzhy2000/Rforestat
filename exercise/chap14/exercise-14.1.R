library(fortedata)
library(randomForest)
library(caret)

data <- fd_soil_respiration()
df <- na.omit(data.frame(
  log_efflux = log(data$soil_co2_efflux),
  soil_temp = data$soil_temp,
  vwc = data$vwc
))

set.seed(123)
ctrl <- trainControl(method = "cv", number = 10)
model_rf <- train(
  log_efflux ~ ., data = df,
  method = "rf",
  trControl = ctrl,
  importance = TRUE
)

print(model_rf)
varImpPlot(model_rf$finalModel)
