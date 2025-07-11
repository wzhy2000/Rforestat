# 加载必要包
library(caret)
library(e1071)
library(tidyverse)

library(forestat)
data("picea")
picea <- na.omit(picea)         # 删除缺失值
str(picea)
picea$AGB <- picea$STEM + picea$BRANCH + picea$FOLIAGE + picea$FRUIT

# 控制参数（10折交叉验证）
set.seed(123)
ctrl <- trainControl(method = "cv", number = 10)

# 核函数设定
kernels <- c("svmLinear", "svmPoly", "svmRadial")
kernel_names <- c("Linear", "Polynomial", "RBF")

# 批量训练 SVR 模型
models <- list()
for (i in seq_along(kernels)) {
  model <- train(
    AGB ~ LH+LCW, data = picea,
    method = kernels[i],
    trControl = ctrl,
    preProcess = c("center", "scale"),
    tuneLength = 5
  )
  models[[kernel_names[i]]] <- model
}

# 汇总交叉验证结果（R²、RMSE 等）
results <- resamples(models)
summary(results)

# 可视化比较
dotplot(results, metric = "RMSE", main = "不同核函数SVR的RMSE比较")
dotplot(results, metric = "Rsquared", main = "不同核函数SVR的R²比较")

# 导出关键性能指标表格
long_results <- results$values %>%
  pivot_longer(cols = starts_with("Linear") | starts_with("Polynomial") | starts_with("RBF"),
               names_to = "Model_Metric",
               values_to = "Value") %>%
  separate(Model_Metric, into = c("Model", "Metric"), sep = "~")

# 计算每个模型的RMSE和Rsquared的平均值
performance_table <- long_results %>%
  filter(Metric %in% c("RMSE", "Rsquared")) %>%
  group_by(Resample, Model, Metric) %>%
  summarise(
    Mean_Value = mean(Value),
    .groups = "drop"
  )

# 输出结果
performance_table
