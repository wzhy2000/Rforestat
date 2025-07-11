library(systemfit)
library(plantecophys)
data(manyacidat)
df <- na.omit(manyacidat)  # 删除缺失值，避免计算错误
# 构建两个方程
eq1 <- Photo ~ Ci + Tleaf + PARi
eq2 <- Ci ~ Photo + Tleaf + PARi
eqs <- list(Photo = eq1, Ci = eq2)
# SUR 拟合
fit_surf <- systemfit(eqs, method = "SUR", data = df)
summary(fit_surf)
model_eval <- function(model) {
  for (i in 1:length(model$eq)) {
    name <- names(model$eq)[i]
    y <- model$eq[[i]]$model[[1]]
    yhat <- fitted(model$eq[[i]])
    res <- residuals(model$eq[[i]])
    SSE <- sum(res^2)
    SST <- sum((y - mean(y))^2)
    R2 <- 1 - SSE / SST
    RMSE <- sqrt(mean(res^2))
    MSE <- mean(res^2)
    cat(paste0("\n[", name, "]\n"))
    cat(sprintf("  R²    = %.4f\n", R2))
    cat(sprintf("  MSE   = %.4f\n", MSE))
    cat(sprintf("  RMSE  = %.4f\n", RMSE))
  }
}
model_eval(fit_surf)
par(mfrow = c(2, 2))  # 设置2行2列图像
for (i in 1:2) {
  name <- names(fit_surf$eq)[i]
  y <- fit_surf$eq[[i]]$model[[1]]
  yhat <- fitted(fit_surf$eq[[i]])
  res <- residuals(fit_surf$eq[[i]])
  plot(y, yhat, main = paste("拟合图 -", name),
       xlab = "Observed", ylab = "Fitted", pch = 16, col = "steelblue")
  abline(0, 1, col = "red", lty = 2)
  
  # 残差图
  plot(yhat, res, main = paste("残差图 -", name),
       xlab = "Fitted", ylab = "Residuals", pch = 16, col = "gray40")
  abline(h = 0, col = "red", lty = 2)
}
par(mfrow = c(1, 1))  # 恢复默认图像布局
