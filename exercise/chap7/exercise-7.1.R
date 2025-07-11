library(ggplot2)
library(forestat)
data("larch")
# 建立 Gamma-log GLM
mod_cw <- glm(CW ~ H, family = Gamma(link = "log"), data = larch)
summary(mod_cw)                       # 查看系数与显著性
# 预测并绘图
larch$pred <- predict(mod_cw, type = "response")
ggplot(larch, aes(H, CW)) +
  geom_point(colour = "grey50") +
  geom_line(aes(y = pred), colour = "royalblue", linewidth = 1) +
  labs(title = "Gamma-GLM：树高对冠幅平均宽度的影响",
       x = "Height (m)", y = "CW (m)")
# 拟合优度指标
deviance(mod_cw);  AIC(mod_cw)
