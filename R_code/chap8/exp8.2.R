library("openxlsx")
library(nlme)

# 其中生物量是Stem，Branch，Foliage和Fruit之和
data2<-read.xlsx("D3.xlsx",sheet = 1, colNames = TRUE)

set.seed(1)
datapartde <-sample(2, nrow(data2), replace = TRUE, prob = c(0.7,0.3))
trainde <- data2[datapartde==1,]
testde <- data2[datapartde==2,]

dim(trainde)
dim(testde)


FittingEvaluationIndex <- function(EstiH, ObsH) {
  Index<-array(dim=6)
  e<-ObsH-EstiH
  e1<-ObsH-mean(ObsH)
  pe<-mean(e)
  var2<-var(e)
  var<-sqrt(var(e))
  RMSE<-sqrt(pe^2+var2)
  R2<-1-sum(e^2)/sum((e1)^2)
  TRE<-100*sum(e^2)/sum((EstiH)^2)
  Index[1]<-pe
  Index[2]<-RMSE
  Index[3]<-R2
  Index[4]<-var2
  Index[5]<-TRE
  Index[6]<-var
  dimnames(Index)<-list(c("pe","RMSE","R2","Var","TRE","sd"))
  return(Index)
}


# 幂函数（异速生长模型）
fit1<-nls(生物量~a*胸径^b+c*树高^d, start=c(a=1,b=1,c=1,d=1), data=trainde)
summary(fit1)
AIC(fit1)
BIC(fit1)
FittingEvaluationIndex(predict(fit1, newdata = trainde), trainde$生物量)
FittingEvaluationIndex(predict(fit1, newdata = testde), testde$生物量)



# 指数函数
fit2<-nls(生物量~a*exp(b*胸径+c*树高), start=c(a=1,b=0.1,c=0.1),data=trainde)
summary(fit2)
AIC(fit2)
BIC(fit2)
FittingEvaluationIndex(predict(fit2, newdata = trainde), trainde$生物量)
FittingEvaluationIndex(predict(fit2, newdata = testde), testde$生物量)

# Richard
fit.richard<-nls(生物量~a*(1-exp(-b*胸径+c*树高)), start=c(a=1,b=-0.1,c=0.1), data=trainde)
summary(fit.richard)
AIC(fit.richard)
BIC(fit.richard)
FittingEvaluationIndex(predict(fit.richard, newdata = trainde), trainde$生物量)
FittingEvaluationIndex(predict(fit.richard, newdata = testde), testde$生物量)

