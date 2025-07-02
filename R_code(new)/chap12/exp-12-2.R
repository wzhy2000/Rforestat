library(systemfit)
library(forestat)

options(digits = 4)
data(picea)

attach(picea)

DBH <- D0
AGB <- STEM + BRANCH + FOLIAGE + FRUIT

NDBH <- DBH ~ beta1 * exp(-beta2 * LH - beta3 * CPA)
NAGB <- AGB ~ alpha1 * DBH^alpha2 * LH^alpha3

models <- list(NDBH, NAGB)
startvalues <- c(
  beta1 = 1, beta2 = 0.1, beta3 = 0.1,
  alpha1 = 1, alpha2 = 1, alpha3 = 0.1
)
instrument <- ~ DBH + AGB

modele3.sur <- nlsystemfit(method = "SUR", models, startvalues, data = picea)
modele3.2sls <- nlsystemfit(method = "2SLS", models, startvalues, inst = instrument, data = picea)



