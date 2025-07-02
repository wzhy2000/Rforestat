library(systemfit)

train.up5.HCB <- read.csv("data-{train.up5.hcb}-exp11-1.CSV",sep = ",", fileEncoding = "GBK")
test.up5.HCB <- read.csv("data-{test.up5.hcb}-exp11-1.CSV",sep = ",", fileEncoding = "GBK")
t.up5 <- train.up5.HCB

attach(t.up5)

NH <- H~1.3+a0*exp(-a1*exp(-a2*D))
NHCB <- HCB~H/(1+exp(b0+b1*D+b2*cw))
NCL <- CL~c0/(1+c1*exp(-c2*D))

models <- list(NH,NHCB,NCL)
startvalues <- c(a0=27.8,a1=2.85,a2=0.08,b0=2,b1=0.3,b2=0.5,c0=6,c1=3,c2=0.08)
instrument <- ~D+H+cw

modele1.sur <- nlsystemfit("SUR", models, startvalues, data = train.up5.HCB, eqnlabels = list("H", "HCB", "CL"))
modele1.2sls <- nlsystemfit("2SLS", models, startvalues, data = train.up5.HCB, inst = instrument)
modele1.3sls <- nlsystemfit("3SLS", models, startvalues, data = train.up5.HCB, inst = instrument)

summary(modele1.sur)

coefs <- rbind(modele1.sur$b, modele1.2sls$b, modele1.3sls$b)
rownames(coefs) <- list("SUR", "2SLS", "3SLS")
print(coefs)

 
NH <- H ~ (c0 / (1 + c1 * exp(-c2 * D))) / (1 - 1 / (1 + exp(b0 + b1 * D + b2 * cw)))
NHCB <- HCB ~ H / (1 + exp(b0 + b1 * D + b2 * cw))
NCL <- CL ~ c0 / (1 + c1 * exp(-c2 * D))
models <- list(NH, NHCB, NCL)
startvalues <- c(b0 = 2, b1 = 0.3, b2 = 0.5, c0 = 6, c1 = 3, c2 = 0.08)
modele2.sur <- nlsystemfit("SUR", models, startvalues, data = train.up5.HCB, eqnlabels = list("H", "HCB", "CL"))

print(modele1.sur$b)
print(modele2.sur$b)
print(modele1.sur$covb)
print(modele2.sur$covb)


library(ggplot2)
data <- data.frame(Method = c(rep("Simultaneous", length(modele1.sur$resids)), rep("Additive", length(modele2.sur$resids))), X = c(rep("H", length(modele1.sur$resids[, 1])), rep("HCB", length(modele1.sur$resids[, 2])), rep("CL", length(modele1.sur$resids[, 3])), rep("H", length(modele2.sur$resids[, 1])), rep("HCB", length(modele2.sur$resids[, 2])), rep("CL", length(modele2.sur$resids[, 3]))), Residuals = c(modele1.sur$resids[, 1], modele1.sur$resids[, 2], modele1.sur$resids[, 3], modele2.sur$resids[, 1], modele2.sur$resids[, 2], modele2.sur$resids[, 3]))
p <- ggplot(data, aes(X, Residuals, fill = Method)) +
  geom_boxplot()
ggsave("plot.png", plot = p, height = 5, units = "in")
