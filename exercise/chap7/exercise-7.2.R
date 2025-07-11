library(dplyr); 
library(ggplot2)
library(forestat)
data("larch")
# 1) 构造健康状态变量
larch$healthy <- ifelse(larch$LIFE %in% c("alive", "healthy"), 1, 0)
# 2) 按样地聚合，得到 success / failure 及 SD 均值
plot_larch <- larch %>%
  group_by(PLOT) %>%
  summarise(healthy = sum(healthy),
            total   = n(),
            SD_mean = mean(SD, na.rm = TRUE))
# 3) 拟合 quasibinomial GLM
mod_health <- glm(cbind(healthy, total - healthy) ~ SD_mean,
                  family = quasibinomial(link = "logit"),
                  data   = plot_larch)
summary(mod_health)
# 4) 预测并绘图
plot_larch$pred <- predict(mod_health, type = "response")
ggplot(plot_larch, aes(SD_mean, healthy/total)) +
  geom_point(colour = "darkgrey", size = 2) +
  geom_line(aes(y = pred), colour = "forestgreen", linewidth = 1) +
  labs(title = "Quasi-binomial GLM：林分密度对健康树比例的影响",
       x = "SD_mean (m²·ha⁻¹)", y = "Healthy proportion")
