library(forestat)
library(caret)
library(pROC)
data("larch")

table(larch$life)
set.seed(123)
# 移除 unknown 数据
larch <- larch[larch$life != "unknown", ]
# 重新定义枯死状态的二分类变量（0 表示存活，1 表示枯死）
larch$is_dead <- ifelse(larch$life %in% c("broken_head", "broken_tip"), 1, 0)
larch$is_dead <- as.factor(larch$is_dead)
# 检查新变量分布
table(larch$is_dead)
# 将因子变量转为数值变量
larch$is_dead <- as.numeric(as.character(larch$is_dead))

model_poisson <- glm(is_dead ~ H + D + CBH + CW, 
                     family = poisson(link = "log"), 
                     data = larch)
summary(model_poisson)

# model_binomial <- glm(is_dead ~ H + D + CBH + CW, 
#                    family = binomial(link = "logit"), 
#                    data = larch)
# summary(model_binomial)

# 枯死概率
larch$predicted_prob <- predict(model_poisson, type = "response")
head(larch$predicted_prob)

# 将概率转换为二分类预测结果
larch$predicted_class <- ifelse(larch$predicted_prob > 0.5, 1, 0)
larch$predicted_class <- as.factor(larch$predicted_class)

# 生成混淆矩阵
confusionMatrix(larch$predicted_class, larch$is_dead)

roc_curve <- roc(larch$is_dead, larch$predicted_prob)
plot(roc_curve)
auc(roc_curve)  # 计算 AUC


