# 问题一
library(forestat)
data(birch)
H <- birch$H
breaks <- seq(min(H), max(H) + 1, by = 1)
obs <- table(cut(H, breaks, right = FALSE))
obs <- as.vector(obs)
hp.mu <- 10; hp.sigma <- 4
theory.probs <- pnorm(breaks[-1], mean = hp.mu, sd = hp.sigma) - 
  pnorm(breaks[-length(breaks)], mean = hp.mu, sd = hp.sigma)
print(obs)
obs.combed <- c(obs[1:19], sum(obs[20:length(obs)]))
theory.combed <- c(theory.probs[1:19], sum(theory.probs[20:length(theory.probs)]))
theory.combed <- theory.combed / sum(theory.combed)
chisq.ret <- chisq.test(obs.combed, p = theory.combed)
print(chisq.ret)

# 问题二
exp.mu <- mean(H) 
exp.sigma <- sd(H) 
theory.probs <- pnorm(breaks[-1], mean = exp.mu, sd = exp.sigma) - 
  pnorm(breaks[-length(breaks)], mean = exp.mu, sd = exp.sigma)
theory.combed <- c(theory.probs[1:19], sum(theory.probs[20:length(theory.probs)]))
theory.combed <- theory.combed / sum(theory.combed)
chisq.ret2 <- chisq.test(obs.combed, p = theory.combed)
cat("Chi-squared statistic:", chisq.ret2$statistic, "\n",
    "P-value:", chisq.ret2$p.value, "\n",  "Degrees of freedom:", df, "\n")

# 问题三
set.seed(123)
H.perturbed <- H + rnorm(length(H), mean = 0, sd = 1e-6) 
ks.ret <- ks.test(H.perturbed, "pnorm", mean = exp.mu, sd = exp.sigma)
print(ks.ret)

# 问题四
data(larch)
H.birch <- birch$H + runif(length(birch$H), min = -1e-6, max = 1e-6)
H.larch <- larch$H + runif(length(larch$H), min = -1e-6, max = 1e-6)
ks.ret2 <- ks.test(H.birch, H.larch)
print(ks.ret2)

# 问题五
sw.ret <- shapiro.test(H.birch)
print(sw.ret)