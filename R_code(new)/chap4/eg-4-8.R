# 问题一
library(forestat)
data(birch)
H <- birch$H
inner.breaks <- seq(floor(min(H)), ceiling(max(H)), by = 1)
breaks <- c(-Inf, inner.breaks, Inf)
obs <- as.vector(table(cut(H, breaks = breaks, right = TRUE)))
hp.mu <- 10; hp.sigma <- 4
theory.probs <- pnorm(breaks[-1], mean = hp.mu, sd = hp.sigma) - 
  pnorm(breaks[-length(breaks)], mean = hp.mu, sd = hp.sigma)
print(obs)

merge_tail_bins <- function(obs, prob, min_exp = 5) {
  exp <- sum(obs) * prob
  
  while (length(exp) > 2 && exp[1] < min_exp) {
    obs[2] <- obs[1] + obs[2]
    prob[2] <- prob[1] + prob[2]
    obs <- obs[-1]
    prob <- prob[-1]
    exp <- sum(obs) * prob
  }
  
  while (length(exp) > 2 && tail(exp, 1) < min_exp) {
    j <- length(exp)
    obs[j - 1] <- obs[j - 1] + obs[j]
    prob[j - 1] <- prob[j - 1] + prob[j]
    obs <- obs[-j]
    prob <- prob[-j]
    exp <- sum(obs) * prob
  }
  
  stopifnot(all(exp >= min_exp))
  list(obs = obs, prob = prob)
}

merged <- merge_tail_bins(obs, theory.probs)
obs.combined <- merged$obs
theory.combined <- merged$prob
chisq.ret <- chisq.test(obs.combined, p = theory.combined)
print(chisq.ret)

# 问题二
# exp.mu <- mean(H) 
# exp.sigma <- sd(H) 
# theory.probs <- pnorm(breaks[-1], mean = exp.mu, sd = exp.sigma) - 
#   pnorm(breaks[-length(breaks)], mean = exp.mu, sd = exp.sigma)
# 
# theory.combined <- c(theory.probs[1:19], sum(theory.probs[20:length(theory.probs)]))
# theory.combined <- theory.combined / sum(theory.combined)
# stat <- sum((obs.combined - sum(obs.combined) * theory.combined)^2 / 
#               (sum(obs.combined) * theory.combined))
# df <- length(obs.combined) - 2 - 1
# p.value <- pchisq(stat, df = df, lower.tail = FALSE)
# cat("Chi-squared statistic:", stat, "\n",
#     "P-value:", p.value, "\n",
#     "Degrees of freedom:", df, "\n")


exp.mu <- mean(H)
exp.sigma <- sd(H)

theory.probs <- pnorm(breaks[-1], mean = exp.mu, sd = exp.sigma) -
  pnorm(breaks[-length(breaks)], mean = exp.mu, sd = exp.sigma)

# 用估计参数后的理论概率，重新合并；二者会保持一一对应
merged <- merge_tail_bins(obs, theory.probs)

obs.combined <- merged$obs
theory.combined <- merged$prob
theory.combined <- theory.combined / sum(theory.combined)

# 防止以后修改代码时再次出现“向量回收”
stopifnot(length(obs.combined) == length(theory.combined))

stat <- sum((obs.combined - sum(obs.combined) * theory.combined)^2 /
              (sum(obs.combined) * theory.combined))

df <- length(obs.combined) - 2 - 1

p.value <- pchisq(stat, df = df, lower.tail = FALSE)

cat("Chi-squared statistic:", stat, "\n",
    "P-value:", p.value, "\n",
    "Degrees of freedom:", df, "\n")




# 问题三
library(nortest)
lillie.ret <- lillie.test(H)
print(lillie.ret)

# 问题四
data(larch)
H.birch <- birch$H
H.larch <- larch$H
ks.ret2 <- ks.test(H.birch, H.larch, exact = FALSE)
print(ks.ret2)

# 问题五
sw.ret <- shapiro.test(H.birch)
print(sw.ret)
