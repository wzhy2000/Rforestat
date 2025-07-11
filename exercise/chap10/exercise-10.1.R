library(vegan)
library(nlme)
library(ggplot2)

data(mite)
data(mite.env)

# 计算 Shannon 多样性
shannon <- diversity(mite)
df <- data.frame(Diversity = shannon, WatrCont = mite.env$WatrCont)
mod_pow_gnls <- gnls(Diversity ~ a * WatrCont^b,
                     data = df,
                     start = list(a = 1, b = 0.1))

summary(mod_pow_gnls)
mod_exp_gnls <- gnls(Diversity ~ a * exp(b * WatrCont),
                     data = df,
                     start = list(a = 1, b = 0.01))

summary(mod_exp_gnls)
# AIC
AIC(mod_pow_gnls)
AIC(mod_exp_gnls)

# R² 自定义函数
calc_r2_gnls <- function(model, data) {
  y_obs <- data$Diversity
  y_pred <- predict(model)
  ss_tot <- sum((y_obs - mean(y_obs))^2)
  ss_res <- sum((y_obs - y_pred)^2)
  1 - ss_res / ss_tot
}

calc_r2_gnls(mod_pow_gnls, df)
calc_r2_gnls(mod_exp_gnls, df)
df$pow_gnls <- predict(mod_pow_gnls)
df$exp_gnls <- predict(mod_exp_gnls)
library(tidyr)
df_long <- pivot_longer(df, cols = c(pow_gnls, exp_gnls),
                        names_to = "Model", values_to = "Fitted")

ggplot(df, aes(x = WatrCont, y = Diversity)) +
  geom_point(size = 2, color = "black") +
  geom_line(data = df_long, aes(x = WatrCont, y = Fitted, color = Model, linetype = Model), size = 1.2) +
  scale_color_manual(values = c("pow_gnls" = "red", "exp_gnls" = "green"),
                     labels = c("幂函数拟合", "指数函数拟合")) +
  scale_linetype_manual(values = c("pow_gnls" = "dashed", "exp_gnls" = "dotted"),
                        labels = c("幂函数拟合", "指数函数拟合")) +
  labs(title = "nlme::gnls 非线性模型拟合效果",
       x = "土壤水分 WatrCont (%)",
       y = "Shannon多样性",
       color = "拟合模型", 
       linetype = "拟合模型") +
  theme_minimal(base_size = 14)
