library(forestat)
library(e1071)

data(picea)

picea$AGB <- picea$STEM + picea$BRANCH + picea$FOLIAGE + picea$FRUIT

set.seed(123)
idx.train <- sample(nrow(picea), floor(0.7 * nrow(picea)))
picea.train <- picea[idx.train, ]
picea.test <- picea[-idx.train, ]
x.train <- picea.train[, c("LH", "LHCB", "CPA", "D0", "H0", "HCB0", "CW")]
y.train <- picea.train$AGB
x.test <- picea.test[, c("LH", "LHCB", "CPA", "D0", "H0", "HCB0", "CW")]
y.test <- picea.test$AGB

## 需要一些时间运行
modela.svm.tune <- tune.svm(
  AGB ~ LH + LHCB + CPA + D0 + H0 + HCB0 + CW,
  data = picea.train,
  type = "eps-regression",
  kernel = "radial",
  scale = TRUE,
  cost = seq(1, 100, by = 1),
  gamma = seq(0.001, 0.01, by = 0.001),
  epsilon = seq(0.01, 0.1, by = 0.01)
)

modela.svm.best <- modela.svm.tune$best.model

summary(modela.svm.best)


y.pred <- predict(
  modela.svm.best,
  newdata = picea.test
)

FittingEvaluationIndex(y.pred, y.test)

data.svm <- data.frame(x = y.pred, y = y.pred - y.test)
p.svm <- ggplot(data.svm, aes(x = x, y = y)) +
  theme_light() +
  geom_point(color = "steelblue", size = 3, show.legend = F) +
  geom_hline(yintercept = c(0)) +
  geom_vline(xintercept = c(0)) +
  scale_x_continuous(limits = c(0, 600)) +
  scale_y_continuous(limits = c(-10, 10)) +
  labs(x = "地上生物量（kg/株）", y = "预测误差（kg/株）") +
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
pdf("图14.11.pdf", width = 10, height = 6, family = "GB1")
p.svm
dev.off()

