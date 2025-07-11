library(agridat)
library(nlme)
data(hanover.whitepine)
str(hanover.whitepine)
model <- lme(length~ 1, random = ~ 1 | female, data = hanover.whitepine)
summary(model)
varcomp <- VarCorr(model)
female_var <- as.numeric(varcomp[1,1])
resid_var  <- as.numeric(varcomp[2,1])
total_var  <- female_var + resid_var
H2 <- female_var / total_var
cat("广义遗传力 H² =", round(H2, 3), "\n")

