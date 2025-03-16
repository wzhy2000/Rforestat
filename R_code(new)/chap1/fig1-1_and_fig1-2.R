library(forestat)
data(birch)
attach(birch)
boxplot(D, main = "DBH 的箱线图", ylab = "DBH")
# 计算四分位数
Q1 <- quantile(D, 0.25)
Q3 <- quantile(D, 0.75)
IQR <- Q3 - Q1
# 定义异常值的范围
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
# 查找异常值
outliers <- D[D < lower_bound | D > upper_bound]
outliers
clean_birch <- birch[!(birch$D %in% outliers), ]
set.seed(123)
random_samples <- clean_birch[sample(nrow(clean_birch), 10), ]
linear_model <- lm(H ~ D)
predicted_H <- predict(linear_model, newdata = random_samples)
actual_H <- random_samples$H

pdf("图1.1a.pdf", width = 8, height = 8, family = "GB1")
par(mar = c(5, 5.5, 4, 2), mgp = c(3.5, 1, 0))
plot(predicted_H, predicted_H - actual_H, xlab = "树高拟合值(m)",
     ylab = "树高残差(m)", pch = 16, col = "black", cex = 2, 
     cex.lab = 2.2, cex.axis = 2.2)
abline(h = 0, col = "red")
dev.off()

quadratic_model <- lm(H ~ poly(D, 2, raw=TRUE))
predicted_H <- predict(quadratic_model, newdata = random_samples)
actual_H <- random_samples$H

pdf("图1.1b.pdf", width = 8, height = 8, family = "GB1")
par(mar = c(5, 5.5, 4, 2), mgp = c(3.5, 1, 0))
plot(predicted_H, predicted_H - actual_H, xlab = "树高拟合值(m)",
     ylab = "树高残差(m)", pch = 16, col = "black", cex = 2,
     cex.lab = 2.2, cex.axis = 2.2)
abline(h = 0, col = "red")
dev.off()

residuals1 <- residuals(linear_model)  # 计算残差
pdf("图1.2a.pdf", width = 8, height = 8, family = "GB1")
par(mar = c(5, 5.5, 4, 2), mgp = c(3.5, 1, 0))
qqnorm(residuals1, main = "", ylab = "样本分位数", xlab = "理论分位数", cex.axis = 2.2, cex.lab = 2.2)
qqline(residuals1)
dev.off()
residuals2 <- residuals(quadratic_model)
pdf("图1.2b.pdf", width = 8, height = 8, family = "GB1")
par(mar = c(5, 5.5, 4, 2), mgp = c(3.5, 1, 0))
qqnorm(residuals2, main="", ylab = "样本分位数", xlab = "理论分位数", cex.axis = 2.2, cex.lab = 2.2)  # 绘制自相关图
qqline(residuals2)
dev.off()

