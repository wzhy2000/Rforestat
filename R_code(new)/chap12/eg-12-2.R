library("systemfit")
data(ppine)

hg.log.formula <- log(hg) ~ log(tht) + tht^2 + elev + cr
dg.log.formula <- log(dg) ~ log(dbh) + hg + cr + ba
labels <- list("height.log", "diameter.log")
inst <- ~ log(tht) + log(dbh) + elev + cr + ba


options(digits = 5)
model <- list(hg.log.formula, dg.log.formula)
model.ols <- systemfit( model, "OLS", data = ppine)
model.sur <- systemfit( model, "SUR", data = ppine)

print(model.ols)

print(model.sur)

summary(model.ols$eq[[1]])

summary(model.ols$eq[[2]])


model.2sls <- systemfit( model, "2SLS", data = ppine, inst=inst)
print(model.2sls)
model.3sls <- systemfit( model, "3SLS", data = ppine, inst=inst)
summary(model.3sls)

R.restr <- matrix(0, 1, 10)
R.restr[1, 5] <- 1
R.restr[1, 9] <- -1
q.restr <- c(0)
model.ols2 <- systemfit( model, "OLS", data = ppine, restrict.matrix = R.restr, restrict.rhs = q.restr)

print(model.ols2)

hg.formula <- hg ~ exp(h0 + h1 * log(tht) + h2 * tht^2 + h3 * elev + h4 * cr)
dg.formula <- dg ~ exp(d0 + d1 * log(dbh) + d2 * hg + d3 * cr + d4 * ba)
labels <- list("height.growth", "diameter.growth")
inst <- ~ tht + dbh + elev + cr + ba
start.values <- c(h0 = -0.5, h1 = 0.5, h2 = -0.001, h3 = 0.0001, h4 = 0.08, 
                  d0 = -0.5, d1 = 0.009, d2 = 0.25, d3 = 0.005, d4 = -0.02)


model <- list(hg.formula, dg.formula)
nmodel.2sls <- nlsystemfit("2SLS", model, start.values, data = ppine,
                           eqnlabels = labels, inst = inst)
summary(nmodel.2sls)


cbind(b = nmodel.2sls$b, se = nmodel.2sls$se, t = nmodel.2sls$t, p = nmodel.2sls$p)

ypred.2sls <- predict(model.2sls, data=ppine)
ypred.ols <- predict(model.ols, data=ppine)
diff <- ypred.2sls - ypred.ols
summary(diff)


predict.nlsystemfit <- function(model, new_data) {
  coefs <- model$b 
  new_data$hg <- exp(coefs["h0"] + coefs["h1"] * log(new_data$tht) + coefs["h2"] * 
                       new_data$tht^2 + coefs["h3"] * new_data$elev + coefs["h4"] * new_data$cr)
  new_data$dg <- exp(coefs["d0"] + coefs["d1"] * log(new_data$dbh) + coefs["d2"] * 
                       new_data$hg + coefs["d3"] * new_data$cr + coefs["d4"] * new_data$ba)
  return(new_data[, c("hg", "dg")])
}
ypred.2sls <- predict.nlsystemfit(nmodel.2sls, ppine)

summary(ppine[c("hg", "dg")] - ypred.2sls ) 

restrict1 <- "eq2_cr  -  eq1_cr = 0"
linearHypothesis(model.ols, restrict1)

hausman.systemfit(model.2sls, model.3sls)

