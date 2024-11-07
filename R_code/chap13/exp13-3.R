source("load_preprocess.R")
#### 创建GBM模型####
# gbm模型的应用

library(gbm)
j <- seq(100, 5000, by = 100) 
pred <- data.frame()
gbm.HCB<- gbm(agb~.,
              distribution = "gaussian",
              data = trainde_m1,
              n.trees = 4000,
              interaction.depth = 1,
              shrinkage = 0.001,
              bag.fraction = 0.5,
)
plot(gbm.HCB,xlab="迭代次数 Boosting Iterations",ylab="RMSE/m")

gbm_pred1 <- predict(gbm.HCB)
FittingEvaluationIndex(gbm_pred1,trainde_m1$agb)

gbm_pred2 <- predict(gbm.HCB,newdata = testde_m1)
FittingEvaluationIndex(gbm_pred2,testde_m1$agb)

# 4 残差分析
gbm.res <- gbm_pred2-testde_m1$agb
data.gbm<- data.frame(x=gbm_pred2,y=gbm.res)
p.gbm <- ggplot(data.gbm, aes(x=gbm_pred2,y=gbm.res,color="z",cex=2)) + 
  geom_point(,show.legend = F)+geom_hline(yintercept = c(0))+
  labs(x="Ground extimated AGB(g/m2)",y="Residual")
p.gbm
