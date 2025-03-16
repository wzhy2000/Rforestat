library(forestat)
library(caret)
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

modela.knn <- train(x.train, y.train, method = "knn", 
                    preProcess = c("center", "scale"), # 数据标准化
                    tuneLength = 10,                   # 选择 10 个 k 值进行调优
                    trControl = trainControl(method = "cv", number = 10) # 10折交叉验证
)

y.pred1 <- predict(modela.knn) 
FittingEvaluationIndex(y.pred1, y.train)

y.pred2 <- predict(modela.knn, newdata = picea.test) 
FittingEvaluationIndex(y.pred2, y.test)


data.knn <- data.frame(x = y.pred2, y = y.pred2 - y.test)
p.knn <- ggplot(data.knn, aes(x = x, y = y)) + 
  theme_light() +
  geom_point(color = "steelblue", size = 3, show.legend = F) +
  geom_hline(yintercept = c(0)) +
  geom_vline(xintercept = c(0)) +
  scale_x_continuous(limits = c(0, 500)) +
  scale_y_continuous(limits = c(-150, 150)) +
  labs(x = "地上生物量(g/m2)", y = "残差(g/m2)") +
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
pdf("图13.1.pdf", width = 10, height = 6, family = "GB1")
p.knn
dev.off()
