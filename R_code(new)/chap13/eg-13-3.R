library(forestat)
library(brms)
library(ggplot2)
data("larch")

prior.lm <- set_prior("normal(0, 10)", class = "b")
formula.lm <- CW ~ CLR + SD + I(D^2)
model.lm <- brm(formula = formula.lm, data = larch, prior = prior.lm, chains = 4, 
                cores = 4)

model.nlm <- brm(bf(CW ~ p * (a1 + a2 * CLR)/(1 + b1 * exp(-(c1 + c2 * SD) * D)), p ~ 1 + (1 | PLOT), a1 ~ 1, a2 ~ 1, b1 ~ 1, c1 ~ 1, c2 ~ 1, nl = TRUE), data = larch, iter = 4000, warmup = 2000, control = list(adapt_delta = 0.95), chains = 4, cores = 4)

waic.res <- waic(model.lm)
print(waic.res)

bayesr2.res <- bayes_R2(model.lm)
print(bayesr2.res)

ypred.lm <- predict(model.lm, newdata = larch)
ypred.nlm <- predict(model.nlm, newdata = larch)

ppred.lm <- posterior_predict(model.lm, newdata = larch)
ppred.nlm <- posterior_predict(model.nlm, newdata = larch) 

error <- colMeans(ppred.lm)- colMeans(ppred.lm)
p <- ggplot(data.frame(error), aes(x = error)) +
  geom_density(fill = "blue", alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "预测误差", y = "密度") +
  theme_minimal() +
  theme(text = element_text(family = "SimSun"),
        axis.title.x = element_text(size = 19),
        axis.title.y = element_text(size = 19),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18))
plot(p)

linpred.lm <- posterior_linpred(model.lm, newdata = larch)
head(linpred.lm)
linpred.nlm <- posterior_linpred(model.nlm, newdata = larch)
head(linpred.nlm)

hypothesis(model.lm, "CLR = 0")

hypothesis(model.nlm, "c1_Intercept > c2_Intercept")

hypothesis(model.nlm, "b1_Intercept > 0")

pp_check(model.lm, nsamples = 100)
pp_check(model.nlm, nsamples = 100) 

mcmc_plot(model.lm, type = "trace")
mcmc_plot(model.nlm, type = "trace")

plot(model.lm, variable = "b_CLR")
plot(model.nlm, variable = "b_a1_Intercept")

conditional_effects(model.lm, effects = "CLR")
conditional_effects(model.nlm, effects = "CLR")