library(nlme)
library(ggplot2)
library(forestat)
data(larch)

model1 <- lme(H ~ CW + D, random = ~1 | PLOT, data = larch, method = "REML")
summary(model1)
coefs <- intervals(model1, level = 0.95)$fixed
coefs_df <- data.frame(
  Term = rownames(coefs),
  Estimate = coefs[, "est."],
  Lower = coefs[, "lower"],
  Upper = coefs[, "upper"]
)
coefs_df <- subset(coefs_df, Term != "(Intercept)")
ggplot(coefs_df, aes(x = Term, y = Estimate)) +
  geom_point(size = 3, color = "darkblue") +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2, color = "grey40") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "固定效应系数及95%置信区间",
       x = "变量",
       y = "回归系数估计值") +
  theme_minimal()
model2 <- lm(H ~ CW + D, data = larch)
calR2 <- function(y,y_pred){
  SSE <- sum((y - y_pred)^2)
  SST <- sum((y - mean(y))^2)
  return(1-SSE/SST)
}
r2_model1 <- calR2(larch$H,predict(model1))
r2_model2 <- calR2(larch$H,predict(model2))

