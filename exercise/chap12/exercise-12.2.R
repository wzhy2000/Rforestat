library(systemfit)
library(forestat)
data(picea)
picea$AGB <- picea$STEM + picea$BRANCH + picea$FOLIAGE + picea$FRUIT
picea$DBH <- picea$D0

NDBH <- DBH ~ beta1 * exp(-beta2 * LH - beta3 * CPA)
NAGB <- AGB ~ alpha1 * DBH^alpha2 * LH^alpha3
models <- list(NDBH, NAGB)
instrument <- ~LH+CPA

startvalues<-c(beta1=3.119,beta2=-0.219,beta3=-0.044,alpha1=0.930,alpha2=1.737,alpha3=-0.125)
modele3.2sls<-nlsystemfit(method="2SLS",models,startvalues,inst=instrument,
                          data=picea)

modele3.3sls<-nlsystemfit(method="3SLS",models,startvalues,inst=instrument,
                          data=picea)
cat("===== 2SLS 参数估计 =====\n")
print(cbind(Estimate = modele3.2sls$b, StdError = modele3.2sls$se))

cat("\n===== 3SLS 参数估计 =====\n")
print(cbind(Estimate = modele3.3sls$b, StdError = modele3.3sls$se))

# ===== 2SLS 模型结果 =====
cat("【2SLS 模型】\n")
cat("方程 1 - R²:", round(modele3.2sls$eq[[1]]$r2, 5),
    ", RMSE:", round(modele3.2sls$eq[[1]]$rmse, 5), "\n")

cat("方程 2 - R²:", round(modele3.2sls$eq[[2]]$r2, 5),
    ", RMSE:", round(modele3.2sls$eq[[2]]$rmse, 5), "\n")

# ===== 3SLS 模型结果 =====
cat("\n【3SLS 模型】\n")
cat("方程 1 - R²:", round(modele3.3sls$eq[[1]]$r2, 5),
    ", RMSE:", round(modele3.3sls$eq[[1]]$rmse, 5), "\n")

cat("方程 2 - R²:", round(modele3.3sls$eq[[2]]$r2, 5),
    ", RMSE:", round(modele3.3sls$eq[[2]]$rmse, 5), "\n")


par(mfrow = c(2, 2))

plot(modele3.2sls$eq[[1]]$predicted, modele3.2sls$eq[[1]]$residuals,
     xlab = "Fitted DBH", ylab = "Residuals", main = "2SLS - DBH", pch = 16)
abline(h = 0, col = "red")

plot(modele3.2sls$eq[[2]]$predicted, modele3.2sls$eq[[2]]$residuals,
     xlab = "Fitted AGB", ylab = "Residuals", main = "2SLS - AGB", pch = 16)
abline(h = 0, col = "red")

plot(modele3.3sls$eq[[1]]$predicted, modele3.3sls$eq[[1]]$residuals,
     xlab = "Fitted DBH", ylab = "Residuals", main = "3SLS - DBH", pch = 16)
abline(h = 0, col = "blue")

plot(modele3.3sls$eq[[2]]$predicted, modele3.3sls$eq[[2]]$residuals,
     xlab = "Fitted AGB", ylab = "Residuals", main = "3SLS - AGB", pch = 16)
abline(h = 0, col = "blue")
