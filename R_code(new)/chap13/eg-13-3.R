library(forestat)
library(brms)
library(ggplot2)
data("larch")

prior.lm <- set_prior("normal(0, 10)", class = "b")
formula.lm <- CW ~ CLR + SD + I(D^2)
model.lm <- brm(formula = formula.lm, data = larch, prior = prior.lm, chains = 4, 
                cores = 4)

prior.nlm <- c(
  set_prior("normal(1, 0.5)", nlpar = "p"),
  set_prior("student_t(3, 0, 0.5)", class = "sd",
            group = "PLOT", nlpar = "p"),
  set_prior("normal(0, 10)", nlpar = "a1"),
  set_prior("normal(0, 10)", nlpar = "a2"),
  set_prior("lognormal(0, 1)", nlpar = "b1"),
  set_prior("normal(0, 2)", nlpar = "c1"),
  set_prior("normal(0, 2)", nlpar = "c2")
)
model.nlm <- brm(
  bf(CW ~ p * (a1 + a2 * CLR) /
       (1 + b1 * exp(-(c1 + c2 * SD) * D)),
     p ~ 1 + (1 | PLOT),
     a1 ~ 1, a2 ~ 1, b1 ~ 1, c1 ~ 1, c2 ~ 1, nl = TRUE),
  data = larch, prior = prior.nlm,
  iter = 4000, warmup = 2000,
  control = list(adapt_delta = 0.95),
  chains = 4, cores = 4
)

# model.nlm <- brm(bf(CW ~ p * (a1 + a2 * CLR)/(1 + b1 * exp(-(c1 + c2 * SD) * D)), p ~ 1 + (1 | PLOT), a1 ~ 1, a2 ~ 1, b1 ~ 1, c1 ~ 1, c2 ~ 1, nl = TRUE), data = larch, iter = 4000, warmup = 2000, control = list(adapt_delta = 0.95), chains = 4, cores = 4)

waic.res <- waic(model.lm)
print(waic.res)

bayesr2.res <- bayes_R2(model.lm)
print(bayesr2.res)

ypred.lm <- predict(model.lm, newdata = larch)
ypred.nlm <- predict(model.nlm, newdata = larch)

ppred.lm <- posterior_predict(model.lm, newdata = larch)
ppred.nlm <- posterior_predict(model.nlm, newdata = larch) 

error <- colMeans(ppred.lm) - larch$CW
p <- ggplot(data.frame(error), aes(x = error)) +
  geom_density(fill = "blue", alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "预测误差", y = "密度") +
  theme_minimal() +
  theme(
    text = element_text(family = "GB1"),
    axis.title.x = element_text(size = 19),
    axis.title.y = element_text(size = 19),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18)
  )

# pdf("图12.4-1.pdf", width = 8, height = 6, family = "GB1")
print(p)
# dev.off()

error <- colMeans(ppred.nlm) - larch$CW
p <- ggplot(data.frame(error), aes(x = error)) +
  geom_density(fill = "blue", alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "预测误差", y = "密度") +
  theme_minimal() +
  theme(
    text = element_text(family = "GB1"),
    axis.title.x = element_text(size = 19),
    axis.title.y = element_text(size = 19),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18)
  )

# pdf("图12.4-2.pdf", width = 8, height = 6, family = "GB1")
print(p)
# dev.off()

epred.lm <- posterior_epred(model.lm, newdata = larch)
head(epred.lm)
epred.nlm <- posterior_epred(model.nlm, newdata = larch)
head(epred.nlm)


hypothesis(model.lm, "CLR = 0")

hypothesis(model.nlm, "c1_Intercept > c2_Intercept")

hypothesis(model.nlm, "b1_Intercept > 0")

# pdf("图12.5-1.pdf", width = 4, height = 3, family = "GB1")
pp_check(model.lm, ndraws = 100)
dev.off()
# pdf("图12.5-2.pdf", width = 4, height = 3, family = "GB1")
pp_check(model.nlm, ndraws = 100)   
# dev.off()

# pdf("图12.6-1.pdf", width = 6, height = 4, family = "GB1")
mcmc_plot(model.lm, type = "trace")
# dev.off()

# pdf("图12.6-2.pdf", width = 6, height = 4, family = "GB1")
mcmc_plot(model.nlm, type = "trace")
# dev.off()

# pdf("图12.7-1.pdf", width = 8, height = 2, family = "GB1")
plot(
  model.lm,
  variable = "b_CLR",
  newpage = FALSE,
  ask = FALSE
)
# dev.off()

# pdf("图12.7-2.pdf", width = 8, height = 2, family = "GB1")
plot(
  model.nlm,
  variable = "b_a1_Intercept",
  newpage = FALSE,
  ask = FALSE
)
# dev.off()

conditional_effects(model.lm, effects = "CLR")
conditional_effects(model.nlm, effects = "CLR")