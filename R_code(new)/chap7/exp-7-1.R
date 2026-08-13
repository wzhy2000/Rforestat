library(agridat)
library(ggplot2)
library(emmeans)
library(forestat)

# 数据加载
data(hanover.whitepine)
str(hanover.whitepine)
summary(hanover.whitepine$length)

pdf("白松主干长度分布.pdf", width = 8, height = 6, family = "GB1")
ggplot(hanover.whitepine, aes(length)) +
  geom_histogram(bins = 15, fill = "steelblue", colour = "white") +
  labs(x = "子代平均上胚轴长度(cm)", y = "数目") +
  theme_minimal() + 
  theme(
    axis.title.x = element_text(size = 26, color = "black"),
    axis.title.y = element_text(size = 26, color = "black"),
    axis.text.x = element_text(size = 26, color = "black"),
    axis.text.y = element_text(size = 26, color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    axis.line = element_line(color = "black", linewidth = 0.8),
    axis.ticks = element_line(color = "black")
  )
dev.off()

# # 数据集划分
train.data <- subset(hanover.whitepine, rep != "R4")
test.data <- subset(hanover.whitepine, rep == "R4")


# 模型构建
# [修订 CH7-148, CH7-149] 纳入试验设计中的重复区组rep
model.gauss <- glm(length ~ rep + female * male, family = gaussian(),
                   data = train.data)
model.gamma <- glm(length ~ rep + female * male, family = Gamma(link = "log"),
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
# [修订 CH7-156] 简化模型仍需保留重复区组rep
model.main <- update(model.gamma, . ~ rep + female + male)
# [修订 CH7-156] Gamma模型的分散参数需要估计，采用F检验
anova.res <- anova(model.main, model.gamma, test = "F")
anova.res
inter.ratio <- anova.res$Deviance[2] / deviance(model.main)
round(inter.ratio, 3)

# 模型测试
# [修订 CH7-148 说明] 模型纳入rep后，测试集区组R4是训练集中未出现的新水平，
# 不能直接predict()。这里对训练集中已观测的区组在连接尺度上取平均，
# 得到与后续emmeans边际均值一致的“平均区组”预测。
rep.levels <- levels(droplevels(train.data$rep))
eta.test <- sapply(rep.levels, function(r) {
  nd <- test.data
  nd$rep <- factor(r, levels = levels(train.data$rep))
  predict(model.gamma, newdata = nd, type = "link")
})
pre.test <- model.gamma$family$linkinv(rowMeans(eta.test))
FittingEvaluationIndex(pre.test, test.data$length)

# [修订 CH7-161] 使用响应尺度边际均值，并在事后比较时明确多重校正
emm <- emmeans(model.gamma, ~ female | male, type = "response")
pairs(emm, adjust = "holm")
plot.df <- as.data.frame(emm)

# 结果可视化
shape.values <- c(
  F193 = 16,
  F195 = 17,
  F197 = 15,
  F201 = 18,
  F203 = 8,
  F204 = 3,
  F208 = 4
)

fit.plot <- ggplot(
  plot.df,
  aes(
    male,
    response,
    colour = female,
    shape = female,
    group = female
  )
) +
  geom_point(size = 3) +
  geom_line() +
  scale_shape_manual(values = shape.values) +
  labs(y = "拟合子代平均上胚轴长度（cm）", x = "父本家系") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 26, color = "black"),
    axis.title.y = element_text(
      size = 26,
      color = "black",
      margin = margin(r = 15)
    ),
    axis.text.x = element_text(size = 26, color = "black"),
    axis.text.y = element_text(size = 26, color = "black"),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20)
  )

pdf(
  "不同父本与母本家系组合下的拟合主干长度.pdf",
  width = 8,
  height = 6,
  family = "GB1"
)
print(fit.plot)
dev.off()
# pdf("不同父本与母本家系组合下的拟合主干长度.pdf", width = 8, height = 6, family = "GB1")
# ggplot(plot.df, aes(male, response, colour = female, group = female)) +
#   geom_point(size = 3) +
#   geom_line() +
#   labs(y = "拟合子代平均上胚轴长度（cm）", x = "父本家系") +
#   theme_minimal() +
#   theme(
#     axis.title.x = element_text(size = 26, color = "black"),  # x轴标题字体大小
#     axis.title.y = element_text(size = 26, color = "black", margin = margin(r = 15)),  # y轴标题字体大小
#     axis.text.x = element_text(size = 26, color = "black"),   # x轴文本字体大小
#     axis.text.y = element_text(size = 26, color = "black"),   # y轴文本字体大小
#     legend.title = element_text(size = 20),  # 图例标题字体大小
#     legend.text = element_text(size = 20)
#     # panel.grid.major = element_blank(),                     # 去掉主网格线
#     # panel.grid.minor = element_blank()
#   )
# dev.off()

# 残差图
# 训练集残差图
res.train <- residuals(model.gamma, type = "response")
pdf("Residuals.train.pdf", width = 8, height = 8, family = "GB1")
par(mar = c(5, 6, 4, 2), mgp = c(3.5, 1, 0))
plot(pre.gamma, res.train, xlab = "拟合子代平均上胚轴长度(cm)",
     ylab = "残差(cm)", pch = 16, col = "black", cex = 1,
     cex.lab = 2, cex.axis = 2)
abline(h = 0, col = "red")
dev.off()


# 测试集残差图
res.test <- test.data$length - pre.test
pdf("Residuals.test.pdf", width = 8, height = 8, family = "GB1")
par(mar = c(5, 6, 4, 2), mgp = c(3.5, 1, 0))
plot(pre.test, res.test, xlab = "拟合子代平均上胚轴长度(cm)",
     ylab = "残差(cm)", pch = 16, col = "black", cex = 1,
     cex.lab = 2, cex.axis = 2)
abline(h = 0, col = "red")
dev.off()



# emm_fam <- emmeans(model.gamma, ~ female, type = "response")
# pairs(emm_fam)
