data("Kmenta")
eqDemand <- consump ~ price + income
eqSupply <- consump ~ price + farmPrice + trend
system <- list(demand = eqDemand, supply = eqSupply)

fitols <- systemfit(system, data = Kmenta)
print(fitols)

modReg <- matrix(0, 7, 6)
colnames(modReg) <- c(
  "demIntercept", "demPrice", "demIncome",
  "supIntercept", "supPrice2", "supTrend"
)
modReg[1, "demIntercept"] <- 1
modReg[2, "demPrice"] <- 1
modReg[3, "demIncome"] <- 1
modReg[4, "supIntercept"] <- 1
modReg[5, "supPrice2"] <- 1
modReg[6, "supPrice2"] <- 1
modReg[7, "supTrend"] <- 1
fitols3 <- systemfit(system, data = Kmenta, restrict.regMat = modReg)
print(fitols3)


Rrestr <- matrix(0, 2, 7)
Rrestr[1, 3] <- 1
Rrestr[1, 7] <- -1
Rrestr[2, 2] <- -1
Rrestr[2, 5] <- 1
qrestr <- c(0, 0.5)
fitols2 <- systemfit(system,
                     data = Kmenta,
                     restrict.matrix = Rrestr, restrict.rhs = qrestr
)
print(fitols2)

