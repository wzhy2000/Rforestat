source("load_preprocess.R")
#  神经网络

# 1. 数据预处理
library(RSNNS)
maxs <- apply(trainde_m1, 2, max) 
mins <- apply(trainde_m1, 2, min)
scaled <- as.data.frame(scale(trainde_m1, center = mins, 
                              scale = maxs - mins))
index <- sample(1:nrow(trainde_m1), round(0.75 * nrow(trainde_m1)))
train_<- scaled[datapartde==1,]
test_<- scaled[datapartde==2,]
nn <- neuralnet(agb~., 
                data = train_, hidden = c(20), 
                linear.output = TRUE)

# 2. 神经网络模型的建立与训练
train_result <- neuralnet::compute(nn,train_[,2:101])$net.result
FittingEvaluationIndex(train_result,train_[,1])
test_result <- neuralnet::compute(nn, test_[,2:101])$net.result
FittingEvaluationIndex(test_result,test_[,1])

# 3. 参数优化
fitControl=trainControl(method="repeatedcv",number=10)
tunedf=expand.grid(.decay=c(0.01:0.1),.size=c(4:15),.bag=TRUE) 
####something wrong here
# caret::train(agb~.,data=train_,method='avNNet',
#              trControl=fitControl,trace=FALSE,
#              linout=FALSE,tuneGrid=tunedf)

nnetmodel.H <- caret::train(agb~.,    
                            data=trainde_m1,method='avNNet',
                            preProc = "range",
                            trace=FALSE,linout=FALSE,tuneGrid=tunedf)

nnetmodel.H

# 4. 模型性能评估
BPNNreg <- mlp(trainde_m1[,2:101], trainde_m1[,1], maxit=100,
               
               inputsTest=testde_m1[,2:101], 
               targetsTest=testde_m1[,1],
               metric="RMSE")
BPNNreg
summary(BPNNreg)
FittingEvaluationIndex(trainde_m1$agb, BPNNreg$fitted.values)

# 5. 模型可视化
#par(cex=0.6)
#plotnet(BPNNreg, pos_col="red", neg_col="grey")
plotIterativeError(BPNNreg, main="BPNN回归迭代误差")
plotRegressionError(trainde_m1$agb, BPNNreg$fitted.values, main="BPNN regssion train fit")

