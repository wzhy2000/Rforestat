# 加载必要库
library(ggplot2)
data(Loblolly)

# 查看数据结构
head(Loblolly)

# 自定义R²函数
calc_r2 <- function(model, data, response) {
  y_obs <- data[[response]]
  y_pred <- predict(model)
  ss_tot <- sum((y_obs - mean(y_obs))^2)
  ss_res <- sum((y_obs - y_pred)^2)
  1 - ss_res/ss_tot
}

# 基础线性模型
mod_lm <- lm(height ~ age, data = Loblolly)

# Logistic模型 nls 拟合
mod_logi <- nls(height ~ A / (1 + exp(-(B + C * age))),
                data = Loblolly,
                start = list(A = 60, B = -1, C = 0.1))

# Gompertz模型 nls 拟合
mod_gomp <- nls(height ~ A * exp(-B * exp(-C * age)),
                data = Loblolly,
                start = list(A = 70, B = 4, C = 0.1))

# 计算AIC
AIC(mod_lm)
AIC(mod_logi)
AIC(mod_gomp)

# 计算R²
r2_lin <- calc_r2(mod_lm, Loblolly, "height")
r2_logi <- calc_r2(mod_logi, Loblolly, "height")
r2_gomp <- calc_r2(mod_gomp, Loblolly, "height")

r2_lin; r2_logi; r2_gomp

# 拟合值加入数据框
Loblolly$pred_lin <- predict(mod_lm)
Loblolly$pred_logi <- predict(mod_logi)
Loblolly$pred_gomp <- predict(mod_gomp)

# 绘制拟合曲线
ggplot(Loblolly, aes(x = age, y = height)) +
  geom_point(size = 2, color = "black") +
  geom_line(aes(y = pred_lin, color = "线性模型"), size = 1.2) +
  geom_line(aes(y = pred_logi, color = "Logistic模型"), linetype = "dashed", size = 1.2) +
  geom_line(aes(y = pred_gomp, color = "Gompertz模型"), linetype = "dotted", size = 1.2) +
  labs(title = "Loblolly 松树高非线性拟合曲线对比",
       x = "树龄 (age)",
       y = "树高 (height, 英尺)",
       color = "拟合模型") +
  theme_minimal(base_size = 14)
