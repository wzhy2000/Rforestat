library(forestat)
library(ggplot2)
data(picea)
df <- na.omit(picea)
lm_model <- lm(D0 ~ LH, data = df)
df$logD <- log(df$D0)
df$logLH <- log(df$LH)
power_model <- lm(logD ~ logLH, data = df)
df$pred_lm <- predict(lm_model)
df$pred_power <- exp(predict(power_model))
cat("线性模型 AIC:", AIC(lm_model), "\n")
cat("幂函数模型 AIC:", AIC(power_model), "\n")
ggplot(df, aes(x = LH, y = D0)) +
  geom_point(alpha = 0.6) +
  geom_line(aes(y = pred_lm), color = "blue", linetype = "dashed") +
  geom_line(aes(y = pred_power), color = "red") +
  labs(title = "胸径预测：线性 vs 幂函数模型", x = "树高 (LH)", y = "胸径 (D0)") +
  theme_minimal()
