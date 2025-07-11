library(nlme)
library(forestat)
data(larch)
larch$PLOT <- as.factor(larch$PLOT)
larch$AgeGroup <- as.factor(larch$AGE.GROUP)
model1 <- lme(H ~ CW + D, random = ~1 | PLOT, data = larch, method = "REML")
model2 <- lm(H ~ CW + D, data = larch)
model3 <- lme(
  fixed = H ~ D + CW,                     # 固定效应
  random = ~1 | AgeGroup/PLOT,                      # AgeGroup 下嵌套 PLOT
  data = larch,
  method = "REML"
)
calR2 <- function(y,y_pred){
  SSE <- sum((y - y_pred)^2)
  SST <- sum((y - mean(y))^2)
  return(1-SSE/SST)
}
r2_model1 <- calR2(larch$H,predict(model1))
r2_model2 <- calR2(larch$H,predict(model2))
r2_model3 <- calR2(larch$H,predict(model3))
model_eval <- data.frame(
  Model = c("model1", "model2", "model3"),
  AIC = c(AIC(model1), AIC(model2), AIC(model3)),
  BIC = c(BIC(model1), BIC(model2), BIC(model3)),
  R_squared = c(r2_model1, r2_model2, r2_model3)
)
print(model_eval)
model_best <- model3
residuals <- resid(model_best, type = "pearson")
fitted_values <- fitted(model_best)
plot(fitted_values, residuals, 
     xlab = "拟合值", ylab = "标准化残差", 
     main = "残差 vs 拟合值图")
abline(h = 0, col = "red", lty = 2)
qqnorm(residuals)
qqline(residuals, col = "red")

