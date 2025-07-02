library(systemfit)
data.train <- read.csv("train_data_exp11-1.csv", sep = ",", fileEncoding = "GBK")
data.test <- read.csv("test_data_exp11-1.csv", sep = ",", fileEncoding = "GBK")
train_set <- train_data

summary(data.train)
set.seed(123)
idx.train <- sample(1:nrow(data1), size = 0.7 * nrow(data1))
data.train <- data1[idx.train, ]
data.test <- data1[-idx.train, ]

eqMsg <- Msg ~ a1 + a2*D^2 + a3*D*H 
eqMsp <- Msp ~ b1 + b2*D^2 + b3*D*H 
eqMsz <- Msz ~ c1 + c2*D^2 + c3*D*H
eqMsy <- Msy ~ d1 + d2*D^2 + d3*D*H
eqMds <- Mds ~ Msg + Msp + Msz + Msy 

model <- list(eqMsg, eqMsp, eqMsz, eqMsy, eqMds)
start.values <- c(a1 = -1, a2 = 0.1, a3 = 0.1,
                  b1 = 0.5, b2 = -0.1, b3 = -0.1,
                  c1 = -0.88, c2 = -0.1, c3 = 0.1,
                  d1 = 1.84, d2 = 0.1, d3 = 0.1)
model.exp1.sur <- nlsystemfit("SUR", model, start.values, data = data.train) 


summary(model.exp1.sur)

coefs <- rbind(model.exp1.sur$b) 
rownames(coefs) <- list("SUR") 
print(coefs)