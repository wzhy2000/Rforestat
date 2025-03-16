source("load_preprocess.R")
# MARS模型的应用
library(dplyr)
library(earth)

# 1. 定义超参数网络
hyper_grid <- expand.grid(
  degree = 1:3,
  nprune = seq(2, 100, length.out = 10) %>% floor()
)

# 2. 模型拟合与超参数调优
cv_H <- caret::train(agb~.,data=trainde_m1,
                     method = "earth",
                     metric = "RMSE",
                     tuneGrid = hyper_grid
)
cv_H

# 4. 调优结果可视化
ggplot(cv_H)+labs(x="nprune",y="RMSE/m")	

# 5. 模型手动拟合
earth.Ha <- earth(agb~., data = trainde_m1,nprune=12,degree=2)

# 6.模型可视化与评估
plot(earth.Ha)

FittingEvaluationIndex(predict(earth.Ha),trainde_m1$agb)  
FittingEvaluationIndex(predict(earth.Ha,newdata = testde_m1),testde_m1$agb)

