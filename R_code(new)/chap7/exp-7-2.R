library(pscl)
library(readxl)
library(ggplot2)
library(caret)
library(pROC)
library(PRROC)   # 用于 PR 曲线和 AUC（需要评分而非标签）


# 数据加载与预处理
df <- read_excel("../../data/case-7.2.xlsx", sheet = 2)
df$Suppressed <- ifelse(df$`V(m3)` < 0.001, 1, 0)

head(df)

prop.table <- with(df, prop.table(table(`Slope position`, Suppressed), 1))
round(prop.table * 100, 3)

# 可视化(可选)
plot.df <- data.frame(Slope = rownames(prop.table),
                      suppress_rate = prop.table[, 2])

# pdf("坡度对抑制苗比例的影响图.pdf", width = 12, height = 6)
ggplot(plot.df, aes(x = Slope, y = suppress_rate)) +
  geom_bar(stat = "identity", fill = "forestgreen") +
  ylab("Suppressed Seedling Rate") +
  xlab("Slope Position") +
  ylim(0, 1) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 26, color = "black"),  # x轴标题字体大小
    axis.title.y = element_text(size = 26, color = "black"),  # y轴标题字体大小
    axis.text.x = element_text(size = 26, color = "black"),   # x轴文本字体大小
    axis.text.y = element_text(size = 26, color = "black"),   # y轴文本字体大小
    # panel.grid.major = element_blank(),                         # 去掉主网格线
    # panel.grid.minor = element_blank()
  )
# dev.off()

# 数据集划分
# [修订 CH7-169, CH7-170, CH7-171] 按Plot分组划分，避免同一样地信息造成数据泄漏
set.seed(123)
plot_ids <- unique(df$Plot)
train_plot_ids <- sample(plot_ids, size = floor(0.7 * length(plot_ids)))
train <- df[df$Plot %in% train_plot_ids, ]
test <- df[!df$Plot %in% train_plot_ids, ]

# 检查训练集和测试集的样本量与阳性率
c(train_n = nrow(train), test_n = nrow(test))
c(train_rate = mean(train$Suppressed == 1),
  test_rate = mean(test$Suppressed == 1))


# 模型构建
model <- glm(Suppressed ~ `Slope position`, family = binomial, data = train)
summary(model)
# [修订 CH7-168, CH7-175] 坡位因子的整体效应用drop1()判断
drop1(model, test = "Chisq")

# 自动寻找最优分类阈值
pre.prob.train <- predict(model, type = "response")
roc.train <- roc(train$Suppressed, pre.prob.train)
best.coords <- coords(roc.train, "best", ret = c("threshold", "specificity", "sensitivity"))
best.threshold <- best.coords["threshold"][[1]]
cat("最优分类阈值为：", best.threshold, "\n")


# 模型性能评估
# [修订 CH7-173] 统一用factor(levels = c(0, 1))构造标签
train$Suppressed <- factor(train$Suppressed, levels = c(0, 1))
pre.class.train <- factor(ifelse(pre.prob.train > best.threshold, 1, 0),
                          levels = c(0, 1))
confusionMatrix(pre.class.train, train$Suppressed, positive = "1")
# 模型测试
pre.prob.test <- predict(model, newdata = test, type = "response")
test$Suppressed <- factor(test$Suppressed, levels = c(0, 1))
pre.class.test <- factor(ifelse(pre.prob.test > best.threshold, 1, 0),
                         levels = c(0, 1))
confusionMatrix(pre.class.test, test$Suppressed, positive = "1")


# 结果可视化

# ROC曲线
roc.train <- roc(train$Suppressed, pre.prob.train)
roc.test <- roc(test$Suppressed, pre.prob.test)
pdf("roc森林培育.pdf", width = 8, height = 8)
plot(roc.train, col = "blue", lwd = 3, ,lty = 1, legacy.axes = TRUE,
     mar = c(6, 6, 2, 2), mgp = c(4, 1, 0),
     cex.lab = 2.2, cex.axis = 2.2)
plot(roc.test, col = "red", lwd = 3, lty = 2, add = TRUE)
legend("bottomright",
       legend = c(paste0("Train AUC = ", round(auc(roc.train), 3)),
                  paste0("Test AUC = ",  round(auc(roc.test),  3))),
       col = c("blue", "red"), lwd = 3, cex = 1.5)
dev.off()

# PR 曲线需要正类概率值 + 实际标签（1表示正类）
# Suppressed == 1 是抑制苗 → 为正类
pr.train <- pr.curve(scores.class0 = pre.prob.train[train$Suppressed == 1],
                     scores.class1 = pre.prob.train[train$Suppressed == 0],
                     curve = TRUE)
pr.test <- pr.curve(scores.class0 = pre.prob.test[test$Suppressed == 1],
                     scores.class1 = pre.prob.test[test$Suppressed == 0],
                     curve = TRUE)
pdf("PR森林培育.pdf", width = 8, height = 8)
par(mar = c(6, 6, 2, 2), mgp = c(4, 1, 0))
plot(pr.train, col = "blue", lwd = 3, lty = 1, auc.main = FALSE,
     cex.lab = 2.2, cex.axis = 2.2, main = "", xlim = c(0, 1), ylim = c(0, 1))
# 添加测试集 PR 曲线
plot(pr.test, col = "red", lwd = 3, lty = 2, add = TRUE)
# [修订 CH7-180] PR曲线的无技巧基线为相应数据集的阳性率，而不是对角线
abline(h = mean(train$Suppressed == "1"), col = "blue", lty = 2)
abline(h = mean(test$Suppressed == "1"), col = "red", lty = 2)
legend("bottomright",
       legend = c(paste0("Train AUC = ", round(pr.train$auc.integral, 3)),
                  paste0("Test AUC = ",  round(pr.test$auc.integral, 3))),
       col = c("blue", "red"), lty = 1, lwd = 3, cex = 1.5)
dev.off()
