# 安装所需的包
# install.packages("ggcorrplot")
# install.packages("ggsci")
library(ggcorrplot)
library(ggsci)
library(forestat)
library(dplyr)


data('picea')

picea <- picea %>% select(-X, -Y, -Z, -`LIDAR-X`, -`LIDAR-Y`, -PLOT1, -OBS, -PLOT)
picea <- apply(picea[, !colnames(picea) %in% c("plot")], 2, function(x) {
  (x - min(x)) / (max(x) - min(x))})
picea <- as.data.frame(picea)

# 查看数据结构
# str(picea)

# 从 picea 数据框中删除列 X, Y, Z
picea <- picea %>% select(-X, -Y, -Z, -`LIDAR-X`, -`LIDAR-Y`, -Plot1, -Obs)

# 绘制相关性热图
ggcorrplot(cor(picea), hc.order = TRUE, type = "lower", outline.color = "white",
           ggtheme = ggplot2::theme_gray, colors = c("#6D9EC1", "white", "#E46726"), lab = T)

# 一个界面绘制四张图
#par(mfrow = c(2, 2), cex = 1.2, cex.axis = 1.2, cex.lab = 1.2, cex.main = 1.2)

# 拟合
model.picea <- lm(LH ~ ., data = picea)
#plot(model.picea)
# 查看模型摘要
#summary(model.picea)

# 逐步回归
model.picea.step <- step(model.picea, direction = "backward")
#plot(model.picea.step)

# 查看模型摘要
#summary(model.picea.step)


# 未逐步回归的模型
round(summary(model.picea)$coefficients, 4)
summary(model.picea)$r.squared
summary(model.picea)$adj.r.squared
summary(model.picea)$fstatistic


# 逐步回归后的模型
round(summary(model.picea.step)$coefficients,4)
summary(model.picea.step)$r.squared
summary(model.picea.step)$adj.r.squared
summary(model.picea.step)$fstatistic


AIC(model.picea)
BIC(model.picea)

AIC(model.picea.step)
BIC(model.picea.step)




