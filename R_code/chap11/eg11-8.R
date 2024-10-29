data("Kmenta")
eqDemand <- consump ~ price + income
eqSupply <- consump ~ price + farmPrice + trend
inst <- ~ income + farmPrice + trend # 工具变量
system <- list(demand = eqDemand, supply = eqSupply)

fit2sls <- systemfit(system, "2SLS", inst = inst, data = Kmenta)
fit3sls <- systemfit(system, "3SLS", inst = inst, data = Kmenta)

h <- hausman.systemfit(fit2sls, fit3sls)
print(h)

