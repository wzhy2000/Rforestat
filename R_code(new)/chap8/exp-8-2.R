#install.packages("nlme")
library(nlme)


# 加载数据
data <- read.csv("data-{sample}-7-1.CSV",sep = ",")
data$Age <- (-1/data$Age + 1/20)
data$Height<-log(data$Height)
data$Type <- as.factor(data$Type)
data$Plot <- as.factor(data$Plot)

# 拟合模型
model.type2 <- lme(Height ~ Age + Type - 1, random = ~1 | Type, data = data)
model.type2
model.plot <- lme(Height ~ Age, random = ~1 | Type / Plot, data = data)
summary(model.plot)

# 参数估计
fixef(model.plot)
ranef(model.plot)
VarCorr(model.plot)

# 假设检验
model.type <- lme(Height ~ Age, random = ~1 | Type, data = data)
lr_test <- anova(model.plot, model.type)
print(lr_test)

# 置信区间
intervals(model.plot, which = "fixed")

intervals(model.plot, which="var-cov")

intervals(model.plot, which="var-cov")$reStruct

