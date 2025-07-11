library(pscl)
data(CO2)
CO2$photosynth_presence <- ifelse(CO2$uptake > 30, 1, 0)
zi_model <- zeroinfl(photosynth_presence ~ conc | conc,
                     data = CO2, dist = "poisson")
hurdle_model <- hurdle(photosynth_presence ~ conc | conc,
                       data = CO2, dist = "poisson")
summary(zi_model)
summary(hurdle_model)
