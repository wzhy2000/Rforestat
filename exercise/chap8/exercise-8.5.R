library(nlme)
data(hanover.whitepine)
mod1 <- lme(length~ 1,
            random = ~1 | female,
            data = hanover.whitepine)
mod2_alt <- lme(length~ 1,
                random = ~1 | female/rep,
                data = hanover.whitepine)
anova(mod1, mod2_alt)
