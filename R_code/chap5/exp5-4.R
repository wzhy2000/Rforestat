library("openxlsx")
library("dplyr")
data=read.xlsx("leidafanyanshuju.xlsx",sheet = 1, colNames = T)
mydata=select(data,LH,LHCB,LCW1,LCW2,LCW,CPA,D0,H0,HCB0,CW01,CW02,CW)  
x=model.matrix(LH~.,mydata)[,-1]
y=mydata$LH

set.seed(0)
train=sample(1:nrow(x)/2)
test=(-train)

library(glmnet)
ridge.mod=glmnet(x[train,],y[train],alpha=0)

dim(coef(ridge.mod))

set.seed(0)
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)

bestlambda=cv.out$lambda.min
bestlambda

ridge.pred=predict(ridge.mod,s=bestlambda,newx = x[test,])
mean((ridge.pred-y[test])^2)

out=glmnet(x,y,alpha=0)
ridge.coef=predict(out,type="coefficient",s=bestlambda)[1:12,]
round(ridge.coef,4)

ridge.pred1=predict(ridge.mod,s=10,newx=x[test,])
mean((ridge.pred1-y[test])^2)

lasso.mod=glmnet(x[train,],y[train],alpha=1)  
set.seed(0)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)

lasso.pred=predict(lasso.mod,s=bestlambda,newx = x[test,])
mean((lasso.pred-y[test])^2)

out=glmnet(x,y,alpha=1)
lasso.coef=predict(out,type="coefficient",s=bestlambda)[1:12,]
round(lasso.coef,4)
lasso.coef[lasso.coef==0]

