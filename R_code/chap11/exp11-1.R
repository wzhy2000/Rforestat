library(systemfit)
library(openxlsx)
train.up5.HCB <- read.xlsx("train.up5.hcb.xlsx",sheet="Sheet 1")
test.up5.HCB <- read.xlsx("test.up5.hcb.xlsx",sheet="Sheet 1")
t.up5 <- train.up5.HCB

attach(t.up5)

NH <- H~1.3+a0*exp(-a1*exp(-a2*D))
NHCB <- HCB~H/(1+exp(b0+b1*D+b2*cw))
NCL <- CL~c0/(1+c1*exp(-c2*D))

models <- list(NH,NHCB,NCL)
startvalues <- c(a0=27.8,a1=2.85,a2=0.08,b0=2,b1=0.3,b2=0.5,c0=6,c1=3,c2=0.08)
instrument <- ~D+H+cw

m0.SUR <- nlsystemfit("SUR", models, startvalues, data=train.up5.HCB , eqnlabels=list("H","HCB","CL"))
m0.2sls <- nlsystemfit("2SLS", models, startvalues, data=train.up5.HCB,inst = instrument)
m0.3sls <- nlsystemfit("3SLS", models, startvalues, data=train.up5.HCB,inst = instrument)

summary(m0.SUR)

coefs <- rbind(m0.SUR$b, m0.2sls$b, m0.3sls$b)
rownames(coefs) <- list("SUR", "2SLS", "3SLS")
print(coefs)

