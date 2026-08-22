library(systemfit)

train.up5.HCB <- read.csv("../../data/case-12.1-train.csv",sep = ",", fileEncoding = "GBK")
test.up5.HCB <- read.csv("../../data/case-12.1-test.csv",sep = ",", fileEncoding = "GBK")
t.up5 <- train.up5.HCB


NH <- H~1.3+a0*exp(-a1*exp(-a2*D))
NHCB <- HCB~H/(1+exp(b0+b1*D+b2*cw))
NCL <- CL~c0/(1+c1*exp(-c2*D))

models <- list(NH,NHCB,NCL)
startvalues <- c(a0=27.8,a1=2.85,a2=0.08,b0=2,b1=0.3,b2=0.5,c0=6,c1=3,c2=0.08)
instrument <- ~D+H+cw

modele1.sur <- nlsystemfit("SUR", models, startvalues, data = train.up5.HCB, eqnlabels = list("H", "HCB", "CL"))
# modele1.2sls <- nlsystemfit("2SLS", models, startvalues, data = train.up5.HCB, inst = instrument)
# modele1.3sls <- nlsystemfit("3SLS", models, startvalues, data = train.up5.HCB, inst = instrument)

summary(modele1.sur)

# coefs <- rbind(modele1.sur$b, modele1.2sls$b, modele1.3sls$b)
# rownames(coefs) <- list("SUR", "2SLS", "3SLS")
# print(coefs)
print(noquote(formatC(modele1.sur$b, format = "f", digits = 5)))

response_names <- c("H", "HCB", "CL")
residuals_sur <- modele1.sur$resids

fit_stats_sur <- data.frame(
  method = "SUR",
  equation = response_names,
  MSE = colMeans(residuals_sur^2),
  RMSE = sqrt(colMeans(residuals_sur^2)),
  R2 = vapply(seq_along(response_names), function(j) {
    y <- train.up5.HCB[[response_names[j]]]
    1 - sum(residuals_sur[, j]^2) / sum((y - mean(y))^2)
  }, numeric(1))
)

print(fit_stats_sur)

 
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
resid_df <- data.frame(
  Method = rep(c("Original system", "Identity-constrained system"),
               each = length(modele1.sur$resids)),
  Equation = rep(rep(c("H", "HCB", "CL"),
                     each = nrow(modele1.sur$resids)), 2),
  Residuals = c(as.vector(modele1.sur$resids),
                as.vector(modele2.sur$resids))
)
p <- ggplot(
  resid_df,
  aes(Equation, Residuals, fill = Method)
) +
  geom_boxplot(colour = "black") +
  scale_fill_manual(values = c("white", "gray70")) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 18, colour = "black"),
    axis.text = element_text(size = 16, colour = "black"),
    legend.title = element_text(size = 16, colour = "black"),
    legend.text = element_text(size = 14, colour = "black")
  )
ggsave("图11.1.pdf", plot = p, height = 5, units = "in")
