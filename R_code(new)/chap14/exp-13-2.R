library(randomForest)
library(forestat)
data("picea")

# 数据处理
picea$AGB = picea$STEM + picea$BRANCH + picea$FOLIAGE + picea$FRUIT

set.seed(123)
idx.train <- sample(nrow(picea), 0.7 * nrow(picea))
picea.train <- picea[idx.train, ]
picea.test <- picea[-idx.train, ]

x.train <- picea.train[, c("LH", "LHCB", "CPA", "D0", "H0", "HCB0", "CW")]
y.train <- picea.train$AGB
x.test <- picea.test[, c("LH", "LHCB", "CPA", "D0", "H0", "HCB0", "CW")]
y.test <- picea.test$AGB

mtry.rsq <- list()
for (i in seq(1, 7, by = 1) ) {
  model.try <- randomForest(x = x.train, y = y.train, ntree = 1000, mtry = i, 
                            importance = TRUE, proximity = TRUE)
  mtry.rsq[[i]] <- model.try$rsq[[1000]]
}
mtry.best <- which.max(mtry.rsq)
cat("mtry=", mtry.best)


# 训练模型
modela.rf <- randomForest(x = x.train, y = y.train, ntree = 1000, mtry = mtry.best, importance = TRUE, proximity = TRUE)
y.pred <- predict(modela.rf, newdata = x.test)
FittingEvaluationIndex(y.pred, y.test)


# 残差图
data.rf <- data.frame(x = y.pred, y = y.pred - y.test)
plot.rf <- ggplot(data.rf, aes(x = x, y = y)) + 
  theme_light() +
  geom_point(color = "steelblue", size = 3, show.legend = F) +
  geom_hline(yintercept = c(0)) +
  geom_vline(xintercept = c(0)) +
  scale_x_continuous(limits = c(0, 600)) +
  scale_y_continuous(limits = c(-50, 50)) +
  labs(x = "地上生物量(g/m2)", y = "残差(g/m2)")+
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
pdf("图13.5.pdf", width = 10, height = 6, family = "GB1")
plot.rf
dev.off()