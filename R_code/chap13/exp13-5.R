source("load_preprocess.R")
# SVM模型调参
library(e1071)
set.seed(1)
obj <- tune.svm(x = trainde_m1[, 2:101], y = trainde_m1[, 1], 
                kernel = 'radial',
                gamma = c(0.00001,0.0001,0.001,0.01,0.1,0.1,c(seq(from = 1, to = 5, by = 1))), 
                cost =c(seq(from = 1, to = 40, by = 1)))
summary(obj)
# dev.new()
# plot(obj)


# 训练SVM模型
controlObject <- trainControl(method = "repeatedcv",
                              repeats = 2,
                              number = 10)
set.seed(1)
svm_model <- svm(x = trainde_m1[, 2:101], y = trainde_m1[, 1], 
                 trControl = controlObject,
                 importance = TRUE,
                 tkernel = 'radial', gamma = 0.001, cost = 13)

# 模型评估
svm_pred1 <- predict(svm_model)
FittingEvaluationIndex(svm_pred1, trainde_m1$agb)

# 在测试集上进行预测和评估
svm_pred2 <- predict(svm_model, newdata = testde_m1[, 2:101])
FittingEvaluationIndex(svm_pred2, testde_m1$agb)

# 绘制残差图
svm.res <- svm_pred2 - testde_m1$agb
data.svm <- data.frame(x = svm_pred2, y = svm.res)
p.svm <- ggplot(testde_m1, aes(x = svm_pred2, y = svm.res)) + 
  theme_light() +
  geom_point(color = "steelblue", size = 3, show.legend = F) +
  geom_hline(yintercept = c(0)) +
  geom_vline(xintercept = c(0)) +
  scale_x_continuous(limits = c(0, 400)) +
  scale_y_continuous(limits = c(-200, 200)) +
  labs(x = "Ground extimated AGB(g/m2)", y = "Residual")
p.svm
