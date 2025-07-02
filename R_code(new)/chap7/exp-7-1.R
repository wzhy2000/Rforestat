library(agridat)
library(ggplot2)
library(emmeans)
library(forestat)

# 数据加载
data(hanover.whitepine)
str(hanover.whitepine)
summary(hanover.whitepine$length)

# pdf("白松主干长度分布.pdf", width = 8, height = 6, family = "GB1")
ggplot(hanover.whitepine, aes(length)) +
  geom_histogram(bins = 15, fill = "steelblue", colour = "white") +
  labs(x = "子代主干长度(cm)", y = "数目") + 
  theme(
    axis.title.x = element_text(size = 26, color = "black"),  # x轴标题字体大小
    axis.title.y = element_text(size = 26, color = "black"),  # y轴标题字体大小
    axis.text.x = element_text(size = 26, color = "black"),   # x轴文本字体大小
    axis.text.y = element_text(size = 26, color = "black"),   # y轴文本字体大小
    # panel.grid.major = element_blank(),                         # 去掉主网格线
    # panel.grid.minor = element_blank()
  )
# dev.off()

# # 数据集划分
train.data <- subset(hanover.whitepine, rep != "R4")
test.data <- subset(hanover.whitepine, rep == "R4")


# 模型构建
model.gauss <- glm(length ~ female * male, family = gaussian(),
               data = train.data)
model.gamma <- glm(length ~ female * male, family = Gamma(link = "log"),
               data = train.data)
summary(model.gamma)


# 性能评估与模型选择
pre.gauss <- predict(model.gauss, type = "response")
FittingEvaluationIndex(pre.gauss, train.data$length)
pre.gamma <- predict(model.gamma, type = "response")
FittingEvaluationIndex(pre.gamma, train.data$length)
AIC(model.gauss, model.gamma)
BIC(model.gauss, model.gamma)

# 模型诊断与显著性检验
model.main <- update(model.gamma, . ~ female + male)
anova.res <- anova(model.main, model.gamma, test = "Chisq")
anova.res        
inter.ratio <- anova.res$Deviance[2] / deviance(model.main)
round(inter.ratio, 3)

# 模型测试
pre.test <- predict(model.gamma, newdata = test.data, type = "response")
FittingEvaluationIndex(pre.test, test.data$length)

emm <- emmeans(model.gamma, ~ female | male, type = "response")
plot.df <- as.data.frame(emm)

# 结果可视化
# pdf("不同父本与母本家系组合下的拟合主干长度.pdf", width = 8, height = 6, family = "GB1")
ggplot(plot.df, aes(male, response, colour = female, group = female)) +
  geom_point(size = 3) +
  geom_line() +
  labs(y = "拟合子代主干长度（cm）", x = "父本家系") +
  theme_minimal() + 
  theme(
    axis.title.x = element_text(size = 26, color = "black"),  # x轴标题字体大小
    axis.title.y = element_text(size = 26, color = "black", margin = margin(r = 15)),  # y轴标题字体大小
    axis.text.x = element_text(size = 26, color = "black"),   # x轴文本字体大小
    axis.text.y = element_text(size = 26, color = "black"),   # y轴文本字体大小
    legend.title = element_text(size = 20),  # 图例标题字体大小
    legend.text = element_text(size = 20) 
    # panel.grid.major = element_blank(),                     # 去掉主网格线
    # panel.grid.minor = element_blank()
  )
# dev.off()

# 残差图
# 训练集残差图
res.train <- residuals(model.gamma, type = "response")
pdf("Residuals.train.pdf", width = 8, height = 8, family = "GB1")
par(mar = c(5, 6, 4, 2), mgp = c(3.5, 1, 0))
plot(pre.gamma, res.train, xlab = "拟合子代主干长度(cm)", 
     ylab = "残差(cm)", pch = 16, col = "black", cex = 1, 
     cex.lab = 2, cex.axis = 2)
abline(h = 0, col = "red")
dev.off()


# 测试集残差图
res.test <- test.data$length - pre.test
pdf("Residuals.test.pdf", width = 8, height = 8, family = "GB1")
par(mar = c(5, 6, 4, 2), mgp = c(3.5, 1, 0))
plot(pre.test, res.test, xlab = "拟合子代主干长度(cm)", 
     ylab = "残差(cm)", pch = 16, col = "black", cex = 1, 
     cex.lab = 2, cex.axis = 2)
abline(h = 0, col = "red")
dev.off()



# emm_fam <- emmeans(model.gamma, ~ female, type = "response")
# pairs(emm_fam)

