data(CO2)
library(ggplot2)
df <- subset(CO2, Plant == "Qn1")
head(df)
ggplot(df, aes(x = conc, y = uptake)) +
  geom_point() +
  labs(title = "Qn1植物CO₂浓度与吸收速率关系", x = "CO₂浓度", y = "吸收速率") +
  theme_minimal()
mm_model <- nls(uptake ~ Vmax * conc / (K + conc),
                data = df,
                start = list(Vmax = 40, K = 300))
summary(mm_model)
log_model <- nls(uptake ~ a + b * log(conc),
                 data = df,
                 start = list(a = 0, b = 1))
summary(log_model)
exp_model <- nls(uptake ~ a * (1 - exp(-b * conc)),
                 data = df,
                 start = list(a = 30, b = 0.01))
summary(exp_model)
calc_r2 <- function(model, data) {
  y <- data$uptake
  y_pred <- predict(model)
  1 - sum((y - y_pred)^2) / sum((y - mean(y))^2)
}

cat("Michaelis-Menten R²:", round(calc_r2(mm_model, df), 3), "\n")
cat("Log 模型 R²:", round(calc_r2(log_model, df), 3), "\n")
cat("指数饱和 R²:", round(calc_r2(exp_model, df), 3), "\n")
df$mm_fit <- predict(mm_model)
df$log_fit <- predict(log_model)
df$exp_fit <- predict(exp_model)

ggplot(df, aes(x = conc, y = uptake)) +
  geom_point(color = "black") +
  geom_line(aes(y = mm_fit), color = "blue", linetype = "solid") +
  geom_line(aes(y = log_fit), color = "green", linetype = "dashed") +
  geom_line(aes(y = exp_fit), color = "red", linetype = "dotted") +
  labs(title = "不同非线性模型拟合对比",
       x = "CO₂浓度", y = "吸收速率") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "green", "red"))
