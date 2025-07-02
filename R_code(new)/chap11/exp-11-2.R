library(dplyr)
library(caret)
library(nlme)
library(forestat)
data <- read.csv("lys-bh.CSV", sep = ",")

data <- select(data, CW, D, H, PLOT, BLOCK, CLR, SD)
head(data)
set.seed(123)
datapartde <- createDataPartition(data$CW, p = 0.7,list = FALSE) 
data.train <- data[datapartde, ]
data.val <- data[ - datapartde, ]


model.CW.BlocK <- nlme(CW ~ (phi1 + phi2 * CLR) / (1 + phi3 * exp(-(phi4 + phi5 * SD) * D)),
                    data = data.train,
                    fixed = phi1 + phi2 + phi3 + phi4 + phi5 ~ 1,
                    random = phi1 ~ 1 | BLOCK / PLOT,
                    start = c(phi1 = 10, phi2 = 0.01, phi3 = 0.08, phi4 = 0.1, phi5 = -0.001))

summary(model.CW.BlocK)

FittingEvaluationIndex(fitted(model.CW.BlocK), data.train$CW)

FittingEvaluationIndex(predict(model.CW.BlocK, data.val), data.val$CW)


model.CW.PLOT <- nlme(CW ~ (phi1 + phi2 * CLR) / (1 + phi3 * exp(-(phi4 + phi5 * SD) * D)),
                  data = data.train,
                  fixed = (phi1 + phi2 + phi3 + phi4 + phi5 ~ 1),
                  random = list(PLOT = pdDiag(phi1 ~ 1)),
                  start = c(phi1 = 7, phi2 = 1, phi3 = 3, phi4 = 0.06, phi5 = -0.00005))

anova(model.CW.PLOT,model.CW.BlocK)


data.train$fit0 <- predict(model.CW.BlocK, newdata = data.train, level = 0)
sst <- sum((data.train$CW - mean(data.train$CW))^2)
ssr <- sum((data.train$fit0 - mean(data.train$CW))^2)
r_squared <- ssr / sst

r_squared


data.train$fit2 <- predict(model.CW.BlocK, newdata = data.train, level = 2)
sst <- sum((data.train$CW - mean(data.train$CW))^2)
ssr <- sum((data.train$fit2 - mean(data.train$CW))^2)
r_squared <- ssr / sst

r_squared











