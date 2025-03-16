data("Kmenta")
eqDemand <- consump ~ price + income
eqSupply <- consump ~ price + farmPrice + trend
system <- list(demand = eqDemand, supply = eqSupply)

fitsur <- systemfit(system, method = "SUR", data = Kmenta)

restrict1 <- "demand_price - supply_farmPrice = 0"
restrict2 <- "demand_price + supply_farmPrice = 0"

res1 <- linearHypothesis(fitsur, restrict1)
res2 <- linearHypothesis(fitsur, restrict2)

