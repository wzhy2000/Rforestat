library(systemfit)
library(forestat)

options(digits = 4)
data(picea)

picea$DBH <- picea$D0
picea$AGB <- with(picea, STEM + BRANCH + FOLIAGE + FRUIT)

NDBH <- DBH ~ beta1 * exp(-beta2 * LH - beta3 * CPA)
NAGB <- AGB ~ alpha1 * DBH^alpha2 * LH^alpha3
models <- list(NDBH, NAGB)
startvalues <- c(beta1 = 1, beta2 = 0.1, beta3 = 0.1, 
                 alpha1 = 1, alpha2 = 1, alpha3 = 0.1)
instrument <- ~ LH + CPA

modele3.sur <- nlsystemfit(method = "SUR", models, startvalues, data = picea)
modele3.2sls <- nlsystemfit(method = "2SLS", models, startvalues, inst = instrument, data = picea)

extract_fit_stats <- function(model) {
  data.frame(
    method = model$method,
    equation = c("DBH", "AGB"),
    mean_error = vapply(model$eq, function(x) mean(x$residuals), numeric(1)),
    RMSE = vapply(model$eq, function(x) x$rmse, numeric(1)),
    R2 = vapply(model$eq, function(x) x$r2, numeric(1))
  )
}

# 估计是否正常结束：code >= 4 表示未收敛
modele3.sur$nlmest$code
modele3.2sls$nlmest$code

fit_stats <- rbind(
  extract_fit_stats(modele3.sur),
  extract_fit_stats(modele3.2sls)
)

print(fit_stats)