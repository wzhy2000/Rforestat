library(gbm)
library(randomForest)
library(caret)
library(forestat)
data(picea)

# 假设 pina 是已加载的数据
set.seed(123)
ctrl <- trainControl(method = "cv", number = 10)
picea$AGB <- picea$STEM + picea$BRANCH + picea$FOLIAGE + picea$FRUIT
# GBM
model_gbm <- train(
  AGB ~ ., data = picea,
  method = "gbm",
  trControl = ctrl,
  verbose = FALSE
)

# RF
model_rf <- train(
  AGB ~ ., data = picea,
  method = "rf",
  trControl = ctrl
)

# 对比
resamples(list(GBM = model_gbm, RF = model_rf)) %>% summary()

# 残差图
preds <- data.frame(
  Actual = picea$AGB,
  GBM = predict(model_gbm, picea),
  RF = predict(model_rf, picea)
)
plot(preds$Actual, preds$GBM - preds$Actual, main = "GBM 残差", ylab = "残差", xlab = "真实值")
