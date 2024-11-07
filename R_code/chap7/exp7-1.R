#install.packages("nlme")
library(nlme)
library(readxl)

# 加载数据
data <- read.csv("data-{sample}-7-1.CSV",sep = ",", header = TRUE)
data$Age <- (-1/data$Age + 1/20)
data$Height<-log(data$Height)
data$Type <- as.factor(data$Type)
data$Plot <- as.factor(data$Plot)

# 拟合模型
fit.lm <- lme(Height ~ Age+Type-1, random = ~1|Type, data=data)
fit.lm

lme.fit2 <- lme(Height ~ Age, random = ~1|Type/Plot, data=data)
summary(lme.fit2)

# 参数估计
fixef(lme.fit2)
ranef(lme.fit2)
VarCorr(lme.fit2)

# 假设检验
fit_reduced <- lme(Height ~ Age, random = ~1|Type, data = data)
lr_test <- anova(lme.fit2, fit_reduced)
print(lr_test)

intervals(lme.fit2, which="fixed")

intervals(lme.fit2, which="var-cov")

intervals(lme.fit2, which="var-cov")$reStruct
