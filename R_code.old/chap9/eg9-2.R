library(mgcv)
set.seed(0)

# Simulate data for the first model (Gaussian family)
dat <- gamSim(1, n=200, scale=2)

# Fit a basic GAM model (Gaussian family)
b <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data=dat)
plot(b, pages=1)
summary(b)

# Simulate data for a Gamma family model
dat <- gamSim(1, n=400, dist="normal", scale=1)
dat$f <- dat$f / 4  # Scale true linear predictor
Ey <- exp(dat$f)
scale <- 0.5  # Mean and GLM scale parameter
dat$y <- rgamma(Ey*0, shape=1/scale, scale=Ey*scale)

# Fit a GAM with Gamma family and log link
bg <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), family=Gamma(link=log), data=dat, method="REML")

# Output the main summary with more detailed information
cat("\n### Model Summary ###\n")
summary(bg)

# 1. Extract and display additional model information

# Adjusted R-squared and Deviance explained
cat("\nAdjusted R-squared:", summary(bg)$r.sq, "\n")
cat("Deviance explained:", summary(bg)$dev.expl, "\n")

# AIC and BIC for model comparison
cat("AIC:", AIC(bg), "\n")
cat("BIC:", BIC(bg), "\n")

# GCV/UBRE score for the model
# cat("GCV/UBRE score:", summary(bg)$gcv.ubre, "\n")

# Smoothing parameters used in the model
cat("\nSmoothing parameters:\n")
print(bg$sp)

# Manually calculate the 5-point summary of the residuals
cat("\nResiduals Summary (5-point summary):\n")
residuals_bg <- residuals(bg)
print(quantile(residuals_bg, probs=c(0, 0.25, 0.5, 0.75, 1)))

# Display the estimated degrees of freedom (EDF) for each smooth term
cat("\nEffective Degrees of Freedom (EDF) for smooth terms:\n")
print(summary(bg)$edf)

# Display REML/ML score (use REML score for REML fit)
cat("\nREML Score:\n")
print(bg$reml)

# Confidence intervals for parametric coefficients using Bayesian posterior covariance
# cat("\nConfidence Intervals for Parametric Coefficients:\n")
# print(confint(bg, parm="all", level=0.95))

# Extracting deviance residuals for checking
cat("\nDeviance Residuals (5-point summary):\n")
deviance_res <- deviance(bg)
print(deviance_res)

# Convergence information
cat("\nConvergence Information:\n")
print(bg$converged)

# Number of iterations during model fitting
cat("\nNumber of iterations:\n")
print(bg$iter)

