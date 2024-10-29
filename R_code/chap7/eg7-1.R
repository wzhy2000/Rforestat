
#例7.1数据加载
data <- read_excel("sample_data.xlsx")
data$Age <- (-1/data$Age + 1/20)
data$Height<-log(data$Height)
data$Type <- as.factor(data$Type)
data$Plot <- as.factor(data$Plot)


#拟合模型
lme.fit1 <- lme(Height ~ Age, random = ~1|Type, data = data)
lme.fit2<- lme(Height ~ Age, random = ~1|Type/Plot, data = data)

#信息提取
summary(lme.fit2)
fixef(lme.fit2)
vcov(lme.fit2)
ranef(lme.fit2)
VarCorr(lme.fit2)
fitted(lme.fit2)
resid(lme.fit2)
predict(lme.fit2)

#假设检验
AIC(lme.fit2)
BIC(lme.fit2)
anova(lme.fit2)
lm.fit <- lm(Height ~ Age,data=data)
lme.fit2<- lme(Height ~ Age, random = ~1|Type/Plot, data = data)

#随机效应假设检验
lme.fit1 <- lme(Height ~ Age, random = ~1|Type, data=data)
lme.fit2 <- lme(Height ~ Age, random = ~1|Type/Plot, data=data)
lr_test <- anova(lme.fit1,lme.fit2)
print(lr_test)
#置信区间
intervals(lme.fit2, which="fixed")
intervals(lme.fit2, which="var-cov")
intervals(lme.fit2, which="var-cov")$reStruct
coef(lme.fit2)


#模型诊断
lme.fit2 <- lme(Height ~ Age,random = ~1|Type/Plot,data=data)
plot(lme.fit2)
qqnorm(lme.fit2,~resid(.))
qqPlot(fit$residuals,id=FALSE)


#设置协方差结构G
lme.fit_pdLogChol <- lme(Height ~ Age, random = ~1|Type/Plot, data=data)
VarCorr(lme.fit_pdLogChol)


lme.fit_pdSymm <- lme(Height ~ Age, random =list(Type=pdSymm(~1),Plot=pdSymm(~1)), data=data)
VarCorr(lme.fit_pdSymm)


lme.fit3 <- lme(Height ~ Age, random=list(Type=pdSymm(~Age)),data=data)
getVarCov(lme.fit3,type="conditional",individuals = "1")


lme.fit4 <- lme(Height ~ Age,random=list(Type=pdDiag(~Age)),data=data)
getVarCov(lme.fit4,type="conditional",individuals = "1")
          
          
#设置方差协方差结构R

fit_corCompSymm <- lme(Heightlog ~ ReciprocalAge, random = ~1|Type/Plot, correlation = corCompSymm(form = ~ 1|Type/Plot), data=data)
VarCorr(fit_corCompSymm)

coef(fit_corCompSymm)

fit_corAR1<- fit_corAR1<- lme(Height ~ Age, random = ~1|Type,correlation = corAR1(form = ~ 1|Type), data=data)
VarCorr(fit_corAR1)
coef(fit_corAR1)








