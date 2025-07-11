library(systemfit)
library(forestat)
df <- picea
df$AGB <- df$STEM + df$BRANCH + df$FOLIAGE + df$FRUIT
df$DBH <- df$D0

NDBH <- DBH ~ beta1 * exp(-beta2 * LH - beta3 * CPA)
NAGB <- AGB ~ alpha1 * DBH^alpha2 * LH^alpha3
models <- list(NDBH, NAGB)

startvalues <- c(
  beta1 = 1, beta2 = 0.1, beta3 = 0.1,
  alpha1 = 1, alpha2 = 1, alpha3 = 0.1
)

modele3.sur<-nlsystemfit(method="SUR",models,startvalues,data=df)
modele3.ols<-nlsystemfit(method="OLS",models,startvalues,data=df)


# 第一个方程（DBH）
cat("【方程 1：DBH 模型】\n")
print(modele3.sur$eq[[1]]$b)       # 参数估计值
print(modele3.sur$eq[[1]]$se)      # 标准误
cat("RMSE: ", modele3.sur$eq[[1]]$rmse, "\n")
cat("R²:   ", modele3.sur$eq[[1]]$r2, "\n\n")

# 第二个方程（AGB）
cat("【方程 2：AGB 模型】\n")
print(modele3.sur$eq[[2]]$b)
print(modele3.sur$eq[[2]]$se)
cat("RMSE: ", modele3.sur$eq[[2]]$rmse, "\n")
cat("R²:   ", modele3.sur$eq[[2]]$r2, "\n\n")

for (i in 1:2) {
  cat(paste0("方程 ", i, ":\n"))
  
  cat("SUR 参数估计:\n")
  print(modele3.sur$eq[[i]]$b)
  cat("SUR 标准误:\n")
  print(modele3.sur$eq[[i]]$se)
  cat("SUR R²: ", modele3.sur$eq[[i]]$r2, "\n")
  cat("SUR RMSE: ", modele3.sur$eq[[i]]$rmse, "\n\n")
  
  cat("OLS 参数估计:\n")
  print(modele3.ols$eq[[i]]$b)
  cat("OLS 标准误:\n")
  print(modele3.ols$eq[[i]]$se)
  cat("OLS R²: ", modele3.ols$eq[[i]]$r2, "\n")
  cat("OLS RMSE: ", modele3.ols$eq[[i]]$rmse, "\n")
  
  cat("-----------------------------\n")
}
