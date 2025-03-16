library(nlme)
library(car)
#例7.1数据加载
data <- read.csv("data-{sample}-7-1.CSV",sep = ",", header = TRUE)
data$Age <- (-1/data$Age + 1/20)
data$Height<-log(data$Height)
data$Type <- as.factor(data$Type)
data$Plot <- as.factor(data$Plot)


#模型构建
model.type <- lme(Height ~ Age, random = ~1 | Type, data = data) 
model.type


model.plot <- lme(Height ~ Age, random = ~1 | Type / Plot, data = data)

#信息提取
summary(model.plot)
fixef(model.plot)
vcov(model.plot)
ranef(model.plot)
VarCorr(model.plot)
AIC(model.plot)
BIC(model.plot)

#模型预测
predict(model.plot)
fitted(model.plot)
resid(model.plot)
coef(model.plot)

#假设检验
anova(model.plot)
model.lm <- lm(Height ~ Age,data=data)
AIC(model.lm)
BIC(model.lm)

model.plot<- lme(Height ~ Age, random = ~1 | Type / Plot, data = data)
AIC(model.plot)
BIC(model.plot)

#随机效应假设检验
model.type <- lme(Height ~ Age, random = ~1 | Type, data = data)
model.plot <- lme(Height ~ Age, random = ~1 | Type / Plot, data = data)
lr_test <- anova(model.type, model.plot)
print(lr_test)

#置信区间
intervals(model.plot, which="fixed")
intervals(model.plot, which="var-cov")
intervals(model.plot, which="var-cov")$reStruct

#模型诊断
model.plot <- lme(Height ~ Age, random = ~1 | Type / Plot, data = data)
plot(model.plot)
qqnorm(model.plot,~resid(.))
qqPlot(model.plot$residuals,id=FALSE)


#设置协方差结构G
model.plot <- lme(Height ~ Age, random = ~1 | Type / Plot, data = data)
VarCorr(model.plot)


model.plot.pdSymm <- lme(Height ~ Age, random =list(Type = pdSymm(~1), Plot = pdSymm(~1)), data = data)
VarCorr(model.plot.pdSymm)


model.plot.pdDiag <- lme(Height ~ Age, random = list(Type = pdDiag(~1), Plot = pdDiag(~1)), data = data)
VarCorr(model.plot.pdDiag)


model.age <- lme(Height ~ Age,random = list(Type = pdDiag(~Age)),data=data)
VarCorr(model.age)
getVarCov(model.age, type = "conditional", individuals = "1")
          
model.age.pdSymm <- lme(Height ~ Age, random = list(Type = pdSymm(~Age)), data = data)          
VarCorr(model.age.pdSymm)
getVarCov(model.age.pdSymm, type = "conditional", individuals = "1")


model.age.pdDiag <- lme(Height ~ Age, random = list(Type = pdDiag(~Age)), data = data)
VarCorr(model.age.pdDiag)
getVarCov(model.age.pdDiag, type = "conditional", individuals = "1")

#设置方差协方差结构R
model.plot <- lme(Height ~ Age, random = ~1 | Type / Plot, data = data)
VarCorr(model.plot)
coef(model.plot)

model.plot.corCompSymm <- lme(Height ~ Age, random = ~1 | Type / Plot, correlation = corCompSymm(form = ~1 | Type / Plot), data = data)
VarCorr(model.plot.corCompSymm)
coef(model.plot.corCompSymm)

model.plot.corAR1 <- lme(Height ~ Age, random = ~1 | Type / Plot, correlation = corAR1(form = ~1 | Type / Plot), data=data)
VarCorr(model.plot.corAR1)
coef(model.plot.corAR1)






