library(forestat)
library(gbm)
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

# 定义参数网格
opt.grid <- expand.grid(interaction.depth = seq(1, 7, by = 1), 
                        shrinkage = seq(0.0005, 0.001, by = 0.0001))
ret <- data.frame()  # 存储结果

# 需要一点时间
for (i in 1:nrow(opt.grid)) {
  depth <- opt.grid$interaction.depth[i]
  shrinkage <- opt.grid$shrinkage[i]
  
  # 训练 GBM 模型
  model <- gbm(formula = AGB ~ LH + LHCB + CPA + D0 + H0 + HCB0 + CW,
                   data = picea.train, 
                   distribution = "gaussian", 
                   n.trees = 10000,
                   interaction.depth = depth, 
                   shrinkage = shrinkage, 
                   cv.folds = 5, 
                   verbose = FALSE)
  # 获取交叉验证误差
  best.trees <- gbm.perf(model, method = "cv", plot.it = FALSE)
  cv.error <- model$cv.error[best.trees]
  
  # 存储结果
  ret <- rbind(ret, data.frame(
    interaction.depth = depth,
    shrinkage = shrinkage,
    best.trees = best.trees,
    cv.error = cv.error
  ))
}

best.params <- ret[which.min(ret$cv.error), ]
print(best.params)

set.seed(123)

#重新构建梯度提升树模型
modela.gbm <- gbm(formula = AGB ~ LH + LHCB + CPA + D0 + H0 + HCB0 + CW,
                  data = picea.train, distribution = "gaussian", n.trees = 10000,
                  interaction.depth = 6, shrinkage = 0.0009, cv.folds = 5, verbose = FALSE)

y.pred <- predict(modela.gbm, picea.test)
FittingEvaluationIndex(y.pred, y.test)


best.trees <- gbm.perf(modela.gbm, method = "cv", plot.it = TRUE)
cat("最佳子树数量为：", best.trees, "\n")

y.resd <- y.pred - y.test
data.gbm <- data.frame(x = y.pred, y = y.resd)
p.gbm <- ggplot(data.gbm, aes(x = x, y = y)) + 
  theme_light() +
  geom_point(color = "steelblue", size = 3, show.legend = F) +
  geom_hline(yintercept = c(0)) +
  geom_vline(xintercept = c(0)) +
  scale_x_continuous(limits = c(0, 600)) +
  scale_y_continuous(limits = c(-50, 50)) +
  labs(x = "地上生物量(g/m2)", y = "残差(g/m2)") +
  theme(
    axis.title.x = element_text(size = 26, color = "black"),  # x轴标题字体大小
    axis.title.y = element_text(size = 26, color = "black"),  # y轴标题字体大小
    axis.text.x = element_text(size = 26, color = "black"),   # x轴文本字体大小
    axis.text.y = element_text(size = 26, color = "black"),   # y轴文本字体大小
    plot.title = element_text(size = 26, color = "black"),     # 图表标题字体大小
    legend.title = element_text(size = 26, color = "black"),   # 图例标题字体大小
    legend.text = element_text(size = 26, color = "black"),      # 图例文本字体大小
    panel.grid.major = element_blank(),                         # 去掉主网格线
    panel.grid.minor = element_blank()                          # 去掉次网格线
  )
pdf("图13.7b.pdf", width = 8, height = 6.5, family = "GB1")
p.gbm
dev.off()
