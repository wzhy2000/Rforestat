library(neuralnet)
library(forestat)
data("crassifolia")

# AGB = Stem + Branch + Foliage + Fruit
crassifolia$AGB = crassifolia$Stem + crassifolia$Branch + crassifolia$Foliage + crassifolia$Fruit
data <- crassifolia[,c("LH", "LHCB", "CPA", "D0", "H0", "HCB0", "CW", "AGB")]
data <- as.data.frame(cbind(scale(data[,-ncol(data)]), AGB = data$AGB))
dim(data)

set.seed(123)
datapartde <- sample(nrow(data), 0.7 * nrow(data))
train_data <- data[datapartde, ]
test_data <- data[-datapartde, ]
train_x <- train_data[, c("LH", "LHCB", "CPA", "D0", "H0", "HCB0", "CW")]
train_y <- train_data$AGB
test_x <- test_data[, c("LH", "LHCB", "CPA", "D0", "H0", "HCB0", "CW")]
test_y <- test_data$AGB

nn_model <- neuralnet(AGB ~ LH+LHCB+CPA+D0+H0+HCB0+CW,         #  LH+LHCB+CPA+D0+H0+HCB0+CW
                      data = train_data,     # 训练数据
                      hidden = c(10, 10),  # 设置为3个隐藏层
                      linear.output = TRUE,  # 设置为TRUE（回归模型）
                      err.fct = "sse",       # 使用平方误差损失函数
                      act.fct = "logistic",  # 激活函数使用logistic（也可以选择tanh等）
                      threshold = 0.1,       # 训练的停止标准，默认值通常足够
                      learningrate = 0.001,   # 学习率为0.01
                      stepmax = 400000
)

# 进行预测
predictions <- predict(nn_model, newdata = test_data)
head(predictions)

# 计算模型的MSE和R²
mse <- mean((predictions - test_y)^2)
rsq <- cor(predictions, test_y)^2
cat("测试集 MSE：", mse, "\n")
cat("测试集 R²：", rsq, "\n")


pdf("neuralnet2.pdf",width = 10,height = 6)
plot(nn_model,
     col.entry = "blue",       # 输入节点颜色
     col.hidden = "green",     # 隐藏层节点颜色
     col.out = "red",          # 输出节点颜色
     arrow.length = 0.2,       # 调整箭头长度
     cex = 0.8,                # 调整节点文本大小
     information = FALSE,
     show.weights = FALSE)                
dev.off()


