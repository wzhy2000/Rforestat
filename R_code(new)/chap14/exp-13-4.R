library(forestat)
library(caret)
library(earth)
library(dplyr)
library(ggplot2)
data("picea")

# 数据处理
# AGB = Stem + Branch + Foliage + Fruit
picea$AGB = picea$STEM + picea$BRANCH + picea$FOLIAGE + picea$FRUIT

# 样本选择
set.seed(123)
idx.train <- sample(nrow(picea), 0.7 * nrow(picea))
picea.train <- picea[idx.train, ]
picea.test <- picea[-idx.train, ]

x.train <- picea.train[, c("LH", "LHCB", "CPA", "D0", "H0", "HCB0", "CW")]
y.train <- picea.train$AGB
x.test <- picea.test[, c("LH", "LHCB", "CPA", "D0", "H0", "HCB0", "CW")]
y.test <- picea.test$AGB

opt.grid <- expand.grid(degree = 1:3, nprune = seq(2, 30, length.out = 29) %>% floor())

tuned.mars <- train(
  AGB ~ LH + LHCB + CPA + D0 + H0 + HCB0 + CW,
  data = picea.train,
  method = "earth", # 使用earth包进行MARS模型拟合
  metric = "RMSE",# 评价指标为准确率
  trControl = trainControl(method = "cv", number = 10), # 10折交叉验证
  tuneGrid = opt.grid # 指定超参数网格
)

best.opt <- tuned.mars$bestTune
print(best.opt)

res <- tuned.mars$results
res$degree <- factor(res$degree)
pdf("图13.8.pdf", width = 8, height = 6, family = "GB1")
ggplot(res, aes(x = nprune, y = RMSE, linetype = degree)) + 
  geom_line() +  # 添加曲线
  labs(x = "nprune", y = "RMSE") + 
  theme_light() +
  scale_y_continuous(limits = c(0, 60)) +  # 设置y轴范围
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +  # 设置不同的线型
  theme(
    axis.title.x = element_text(size = 24, color = "black"),  # x轴标题字体大小
    axis.title.y = element_text(size = 24, color = "black"),  # y轴标题字体大小
    axis.text.x = element_text(size = 24, color = "black"),   # x轴文本字体大小
    axis.text.y = element_text(size = 24, color = "black"),   # y轴文本字体大小
    plot.title = element_text(size = 24, color = "black"),     # 图表标题字体大小
    legend.title = element_text(size = 24, color = "black"),   # 图例标题字体大小
    legend.text = element_text(size = 24, color = "black"),      # 图例文本字体大小
    panel.grid.major = element_blank(),                         # 去掉主网格线
    panel.grid.minor = element_blank()                          # 去掉次网格线
  )
dev.off()

set.seed(123)
model.earth <- earth(AGB ~ LH + LHCB + CPA + D0 + H0 + HCB0 + CW,
                     data = picea.test,
                     degree = 2, 
                     nprune = 8,
                     pmethod = "backward")

y.pred <- predict(model.earth, newdata = x.test)
FittingEvaluationIndex(y.pred, y.test)

# plot(model.earth, cex.lab = 2, cex.axis = 2)
# par(mar = c(5, 5, 4, 2), mgp = c(3.5, 1, 0), mfrow = c(2, 2))
pdf("图13.8a.pdf", width = 8, height = 6, family = "GB1")
par(mar = c(5, 5, 4, 2), mgp = c(3.5, 1, 0))
plot(model.earth, cex.lab = 2, cex.axis = 2, which = 1, main = "")
dev.off()
pdf("图13.8b.pdf", width = 8, height = 6, family = "GB1")
par(mar = c(5, 5, 4, 2), mgp = c(3.5, 1, 0))
plot(model.earth, cex.lab = 2, cex.axis = 2, which = 2, main = "")
dev.off()
pdf("图13.8c.pdf", width = 8, height = 6, family = "GB1")
par(mar = c(5, 5, 4, 2), mgp = c(3.5, 1, 0))
plot(model.earth, cex.lab = 2, cex.axis = 2, which = 3, main = "")
dev.off()
pdf("图13.8d.pdf", width = 8, height = 6, family = "GB1")
par(mar = c(5, 5, 4, 2), mgp = c(3.5, 1, 0))
plot(model.earth, cex.lab = 2, cex.axis = 2, which = 4, main = "")
dev.off()

