# knn_model.R
# k最近邻模型
library(caret)

# 加载数据和预处理函数
source("load_preprocess.R")

grid <- expand.grid(k = seq(1, 20, by = 1))
fitControl <- trainControl(method = "LOOCV", number = 10)

# 训练KNN模型
KNNModel_H <- caret::train(x = scale(trainde_m1[, 2:101]), y = trainde_m1[, 1],
                           data = trainde_m1,
                           method = "knn",
                           tuneGrid = grid,
                           returnData = TRUE,
                           returnResamp = "all",
                           trControl = fitControl,
                           savePredictions = TRUE)
KNNModel_H

# 模型评估
knn_pred1 <- predict(KNNModel_H) 
FittingEvaluationIndex(knn_pred1, trainde_m1$agb)

# 在测试集上进行预测和评估
knn_pred2 <- predict(KNNModel_H, newdata = testde_m1) 
FittingEvaluationIndex(knn_pred2, testde_m1$agb)

# 绘制残差图
knn.res <- knn_pred2 - testde_m1$agb
data.knn <- data.frame(x = knn_pred2, y = knn.res)
p.knn <- ggplot(testde_m1, aes(x = knn_pred2, y = knn.res)) + 
  theme_light() +
  geom_point(color = "steelblue", size = 3
             
             , show.legend = F) +
  geom_hline(yintercept = c(0)) +
  geom_vline(xintercept = c(0)) +
  scale_x_continuous(limits = c(0, 400)) +
  scale_y_continuous(limits = c(-200, 200)) +
  labs(x = "Ground extimated AGB(g/m2)", y = "Residual")
p.knn
