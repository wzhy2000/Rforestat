library(dplyr)
library(caret)
library(nlme)
library(forestat)
data <- read.csv("lys-bh.CSV", sep = ",")
data <- select(data, CW, D, H, PLOT, BLOCK, CLR, SD)
head(data, n = 3)

set.seed(123)
plot.id <- unique(data$PLOT)
train.plot <- sample(plot.id, size = floor(0.7 * length(plot.id)))
data.train <- data[data$PLOT %in% train.plot, ]
data.val <- data[!data$PLOT %in% train.plot, ]

model.CW.BLOCK <- nlme(CW ~ (phi1 + phi2 * CLR) / (1 + phi3 * exp(-(phi4 + phi5 * SD) * D)), 
                       data = data.train, 
                       fixed = phi1 + phi2 + phi3 + phi4 + phi5 ~ 1, 
                       random = phi1 ~ 1 | BLOCK / PLOT, 
                       start = c(phi1 = 10, phi2 = 0.01, phi3 = 0.08, phi4 = 0.1, phi5 = -0.001))


summary(model.CW.BLOCK)

FittingEvaluationIndex(fitted(model.CW.BLOCK), data.train$CW)

pred.val <- predict(model.CW.BLOCK, data.val, level = 0)
FittingEvaluationIndex(pred.val, data.val$CW)


model.CW.PLOT <- nlme(CW ~ (phi1 + phi2 * CLR) / (1 + phi3 * exp(-(phi4 + phi5 * SD) * D)), 
                      data = data.train, 
                      fixed = (phi1 + phi2 + phi3 + phi4 + phi5 ~ 1), 
                      random = list(PLOT = pdDiag(phi1 ~ 1)), 
                      start = c(phi1 = 7, phi2 = 1, phi3 = 3, phi4 = 0.06, phi5 = -0.00005))

anova(model.CW.PLOT, model.CW.BLOCK)


data.train$fit0 <- predict(model.CW.BLOCK, newdata = data.train, level = 0)
sse <- sum((data.train$CW - data.train$fit0)^2)
sst <- sum((data.train$CW - mean(data.train$CW))^2)
rsquared <- 1 - sse / sst
rsquared 


data.train$fit2 <- predict(model.CW.BLOCK, newdata = data.train, level = 2)
sse <- sum((data.train$CW - data.train$fit2)^2)
sst <- sum((data.train$CW - mean(data.train$CW))^2)
rsquared <- 1 - sse / sst
rsquared


