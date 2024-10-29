data("Kmenta")
eqDemand <- consump ~ price + income
eqSupply <- consump ~ price + farmPrice + trend
system <- list(demand = eqDemand, supply = eqSupply)

inst <- ~ income + farmPrice + trend
fit2sls <- systemfit(system, "2SLS", inst = inst, data = Kmenta)
print(fit2sls)

fitols <- systemfit(eqSystem)
print(fitols)

fitsur <- systemfit(eqSystem, method = "SUR")
print(fitsur)

fit3sls <- systemfit(eqSystem,
                     method = "3SLS",
                     inst = ~ income + farmPrice + trend
)
print(fit3sls)



