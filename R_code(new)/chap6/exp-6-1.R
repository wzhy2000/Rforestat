library(ggplot2)
library(forestat)

poplar <- read.csv("tree.CSV", sep = ",")
model.full <- lm(formula = y ~ 1 + x2 + x3 + x4 + x5, data = poplar)
model.full

round(model.full$coefficients, 4)
round(coef(model.full), 4)

summary(model.full)
str(model.full)
names(model.full)
predict(model.full)

# 残差
round(model.full$residuals, 4)
# round(resid(model.full), 4)

# 置信区间
round(confint(model.full), 4)

# 协方差
round(vcov(model.full), 4)

# 预测
predict(model.full, data.frame(x2 = 1, x3 = -1, x4 = -1, x5 = 5.0), interval = "prediction")
predict(model.full, data.frame(x2 = 1, x3 = -1, x4 = -1, x5 = 5.0), interval = "confidence")

# 预测值
y.pred.ori <- predict(model.full)

# 创建一个数据框，包含真实值和预测值
comparison_df <- data.frame(
  True = poplar$y,      # 真实值
  Predicted = y.pred.ori   # 预测值
)

# 绘制散点图
pdf("图5.1a.pdf", width = 8, height = 6, family = "GB1")
ggplot(comparison_df, aes(x = True, y = Predicted)) +
  geom_point(alpha = 1, size = 3) +                             # 添加散点
  geom_smooth(method = "lm", color = "black", se = FALSE) + # 添加线性回归线
  labs(x = "真实值(cm)",
       y = "预测值(cm)") +
  theme_minimal() + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),  # 黑色边框
        panel.grid.major = element_blank(),                         # 去掉主要网格线
        panel.grid.minor = element_blank(),                          # 去掉次要网格线
        axis.title = element_text(size = 20),                       # 设置坐标轴标签字体大小
        axis.text = element_text(size = 20),
        axis.title.y = element_text(size = 20, margin = margin(r = 10))
  )
dev.off()



# 测试x5
model.x5 <- lm(y ~ x5, data = poplar)
summary(model.x5)

# 模型比较
anova(model.x5, model.full)

cex_value <- 2
# 可视化
#pdf("predandconfi.pdf")
attach(poplar)
x5.new <- data.frame(x5 = seq(2, 9, by = 0.25))
y.pred <- predict(model.x5, x5.new, interval = "prediction")
y.conf <- predict(model.x5, x5.new, interval = "confidence")
pdf("图5.1b.pdf", width = 10, height = 7.5, family = "GB1")
attach(poplar)
par(mar = c(4.7, 5.5, 4, 2), mgp = c(3.5, 1, 0))
matplot(x5.new$x5, cbind(y.pred, y.conf[, -1]), type = "l", xlab = "初始苗高(cm)", ylab = "增长量(cm)", lty = c(1, 5, 5, 2, 2), 
        col = c("blue", "red", "red", "black", "black"), lwd = 2, cex.lab = cex_value, cex.axis = cex_value) # 修改字体大小
points(x5, y, cex = cex_value, pch = 20)
legend("topleft", c("样本点", "线性模型", "预测区间", "置信区间"), pch = c(19, NA, NA, NA), 
       lty = c(NA, 1, 5, 2), lwd = c(NA, 2, 2, 2), col = c("black", "blue", "red", "black"), cex = 1.5) # 修改字体大小

dev.off()



















