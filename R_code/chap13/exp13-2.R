source("load_preprocess.R")
#### 随机森林####

# 随机森林模型调参
library(randomForest) 
value <- seq(1, 101, by = 1)
rfModel_bestmtry <- list()
for (i in value) {
  set.seed(1)
  train_rf <- randomForest(x = trainde_m[, 2:102], y = trainde_m[, 1],
                           ntree = 1000, mtry = i, importance = TRUE, proximity = TRUE)
  rfModel_bestmtry[[i]] <- train_rf$rsq[[1000]]
  i <- i + 1
}
best_mtry <- which.max(rfModel_bestmtry)  
print(best_mtry)

# 训练随机森林模型
set.seed(1)
train_rf <- randomForest(x = trainde_m[, 2:102], y = trainde_m[, 1],
                         ntree = 1000, mtry = 17, importance = TRUE, proximity = TRUE)

# 模型评估
FittingEvaluationIndex(predict(train_rf), trainde_m$agb)

# 在测试集上进行预测和评估
test_pred <- predict(train_rf, newdata = testde_m)
FittingEvaluationIndex(test_pred, testde_m$agb)

# 绘制残差图
library(ggplot2)
rf.res <- test_pred - testde_m$agb
data.rf <- data.frame(x = test_pred, y = rf.res)
p.rf <- ggplot(testde_m, aes(x = test_pred, y = rf.res)) + 
  theme_light() +
  geom_point(color = "steelblue", size = 3, show.legend = F) +
  geom_hline(yintercept = c(0)) +
  geom_vline(xintercept = c(0)) +
  scale_x_continuous(limits = c(0, 400)) +
  scale_y_continuous(limits = c(-200, 200)) +
  labs(x = "Ground extimated AGB(g/m2)", y = "Residual")
p.rf
