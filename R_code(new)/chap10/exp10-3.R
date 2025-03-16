library(nnet)
library(dplyr)
library(caret)
library(nlme)
library(forestat)
data <- read.csv("lys-bh.CSV", sep = ",")

data <- select(data, CW, D, H, PLOT, BLOCK, CLR, SD, TYPE)
head(data)

set.seed(123)
datapartde <- createDataPartition(data$CW, p = 0.7,list = FALSE) 
data.train <- data[datapartde, ]
data.val <- data[ - datapartde, ]  


AG.dummy <- as.data.frame(class.ind(data$TYPE))
names(AG.dummy) <- paste0("Type_", 1:length(unique(data$TYPE)))
data.train1 <- data.frame(data, AG.dummy)  
head(data.train1, n=3)

Val.AG.dummy <- as.data.frame(class.ind(data.val$TYPE))
names(Val.AG.dummy) <- paste0("Type_", 1:length(unique(data.val$TYPE)))
data.val1 <- data.frame(data.val, Val.AG.dummy)    
head(data.val1)



model.CW.Type <- nlme(CW ~ (phi11 * Type_1 + phi12 * Type_2 + phi2 * CLR) / (1 + phi3 * exp(-(phi4 + phi5 * SD) * D)),
                  data = data.train1,
                  fixed = (phi11 + phi12 + phi2 + phi3 + phi4 + phi5 ~ 1),
                  random = phi2 ~ 1 | PLOT / BLOCK,
                  start = c(phi11 = 4, phi12 = 6, phi2 = 2, phi3 = 3, phi4 = 0.07, phi5 = -0.00009)) 

summary(model.CW.Type)

FittingEvaluationIndex(fitted(model.CW.Type), data.train1$CW)

FittingEvaluationIndex(predict(model.CW.Type, data.val1), data.val1$CW)



data.train1$fit0 <- predict(model.CW.Type, data.train1, level = 0)
sst <- sum((data.train1$CW - mean(data.train1$CW))^3)
ssr <- sum((data.train1$fit0 - mean(data.train1$CW))^3)
r_squared <- ssr / sst
r_squared

data.train1$fit2 <- predict(model.CW.Type, data.train1, level = 2)
sst <- sum((data.train1$CW - mean(data.train1$CW))^3)
ssr <- sum((data.train1$fit2 - mean(data.train1$CW))^3)
r_squared <- ssr / sst
r_squared
