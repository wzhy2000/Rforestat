library(randomForest)
library(forestat)
data("crassifolia")

# AGB = Stem + Branch + Foliage + Fruit
crassifolia$AGB = crassifolia$Stem + crassifolia$Branch + crassifolia$Foliage + crassifolia$Fruit
set.seed(123)
datapartde <- sample(nrow(crassifolia), 0.7 * nrow(crassifolia))
train_data <- crassifolia[datapartde, ]
test_data <- crassifolia[-datapartde, ]
train_x <- train_data[, c("LH", "LHCB", "CPA", "D0", "H0", "HCB0", "CW")]
train_y <- train_data$AGB
test_x <- test_data[, c("LH", "LHCB", "CPA", "D0", "H0", "HCB0", "CW")]
test_y <- test_data$AGB

randomforest_model <- randomForest(x = train_x, y = train_y, ntree = 500, replace = TRUE, importance = TRUE)

predictions <- predict(randomforest_model, test_x)
mse <- mean((predictions - test_y)^2)
rsq <- cor(predictions, test_y)^2
mse
rsq

pdf("eg_randomforest.pdf",width = 8, height = 5)
# 绘制误差变化图
plot(randomforest_model)
dev.off()

importance(randomforest_model)
