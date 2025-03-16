library(foreach)
library(doParallel)
library(neuralnet)
library(forestat)
library(stringr)
data("picea")

# 数据处理
# AGB = Stem + Branch + Foliage + Fruit
picea$AGB <- picea$STEM + picea$BRANCH + picea$FOLIAGE + picea$FRUIT


set.seed(123)
idx.train <- sample(nrow(picea), 0.7 * nrow(picea))
picea.train <- picea[idx.train, ]
picea.test <- picea[-idx.train, ]

x.train <- picea.train[, c("LH", "LHCB", "CPA", "D0", "H0", "HCB0", "CW")]
y.train <- picea.train$AGB
x.test <- picea.test[, c("LH", "LHCB", "CPA", "D0", "H0", "HCB0", "CW")]
y.test <- picea.test$AGB

picea.test$AGB <- (picea.test$AGB - min(picea.train$AGB)) / (max(picea.train$AGB) - min(picea.train$AGB))
picea.train$AGB <- (picea.train$AGB - min(picea.train$AGB)) / (max(picea.train$AGB) - min(picea.train$AGB))

picea.train <- picea.train[,c("LH", "LHCB", "CPA", "D0", "H0", "HCB0", "CW", "AGB")]
picea.test <- picea.test[,c("LH", "LHCB", "CPA", "D0", "H0", "HCB0", "CW", "AGB")]


# 计算训练集的最大值和最小值
min.train <- apply(picea.train[, -ncol(picea.train)], 2, min)  # 最小值
max.train <- apply(picea.train[, -ncol(picea.train)], 2, max)  # 最大值

# 归一化函数
normalize <- function(x, min_val, max_val) {
  return((x - min_val) / (max_val - min_val))
}

# 对训练集进行归一化（可选）
for (col in names(min.train)) {
  picea.train[[col]] <- normalize(picea.train[[col]], min.train[col], max.train[col])
}

# 对测试集进行归一化
picea.test <- picea.test
for (col in names(min.train)) {
  picea.test[[col]] <- normalize(picea.test[[col]], min.train[col], max.train[col])
}

# 由于该模型的网格搜索时间可能较长，可以通过加载文件 
# “LH+LHCB+CPA+D0+H0+HCB0+CW_2024-12-19_02-19-16.RData” 
# 来直接获取已保存的模型结果，从而节省计算时间，直接跳转到查看结果 print(grid_search_results)
# 并行计算 
# 设置并行计算的核心数
num_cores <- 12  # detectCores()
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# 定义五折交叉验证函数
cross_validate_neuralnet <- function(data, tune_grid, folds = 5) {
  # 创建 K 折
  cv_folds <- createFolds(data$AGB, k = folds, list = TRUE, returnTrain = TRUE)
  
  results <- foreach(params = iter(tune_grid, by = "row"), .combine = rbind, .packages = c("neuralnet", "caret")) %dopar% {
    fold_results <- lapply(cv_folds, function(train_index) {
      # 划分训练集和验证集
      picea.train <- data[train_index, ]
      picea.test <- data[-train_index, ]
      
      # 使用 tryCatch 训练模型和预测
      result <- tryCatch({
        # 训练神经网络模型
        nn_model <- neuralnet(
          AGB ~ LH + LHCB + CPA + D0 + H0 + HCB0 + CW,
          data = picea.train,
          hidden = unlist(params["hidden"]),
          linear.output = TRUE,
          err.fct = "sse",
          act.fct = "logistic",
          threshold = as.numeric(params["threshold"]),
          learningrate = as.numeric(params["learningrate"]),
          stepmax = as.numeric(params["stepmax"])
        )
        
        # 预测
        y.pred <- compute(nn_model, picea.test[, c("LH", "LHCB", "CPA", "D0", "H0", "HCB0", "CW")])$net.result
        
        # 计算性能指标
        rmse <- sqrt(mean((y.pred - picea.test$AGB)^2, na.rm = TRUE))
        r_squared <- cor(y.pred, picea.test$AGB, use = "complete.obs")^2
        
        # 返回正常结果
        return(data.frame(rmse = rmse, r_squared = r_squared))
        
      }, error = function(e) {
        # 如果发生错误，返回 NA 数据框, 返回的是result变量
        return(data.frame(rmse = NA, r_squared = NA))
      })
      
      # 这个返回的是fold_results的某一个元素
      return(result)
    })
    
    
    # 汇总所有折的性能
    fold_results <- do.call(rbind, fold_results)
    if (all(is.na(fold_results$rmse))) {
      avg_rmse <- NA
      avg_r_squared <- NA
    } else {
      avg_rmse <- mean(fold_results$rmse, na.rm = TRUE)
      avg_r_squared <- mean(fold_results$r_squared, na.rm = TRUE)
    }
    
    
    # 返回当前参数组合的平均性能
    data.frame(
      hidden = paste(unlist(params["hidden"]), collapse = ","),
      learningrate = as.numeric(params["learningrate"]),
      threshold = as.numeric(params["threshold"]),
      stepmax = as.numeric(params["stepmax"]),
      avg_rmse = avg_rmse,
      avg_r_squared = avg_r_squared
    )
  }
  
  return(results)
}


# 设置超参数网格
tune_grid <- expand.grid(
  hidden = list(c(10, 10)),  
  learningrate = seq(0.0001, 0.001, by = 0.0001), 
  threshold = c(0.1, 0.01, 0.001, 0.0001),
  stepmax = c(500000, 1200000))




# 执行网格搜索
grid_search_results <- cross_validate_neuralnet(data = picea.train, 
                                                tune_grid = tune_grid, 
                                                folds = 10)

# 停止多核并行
stopCluster(cl)

# 保存工作空间
current_time <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
file_name <- paste0("LH+LHCB+CPA+D0+H0+HCB0+CW_", current_time, ".RData")  # 需要修改
save(grid_search_results, file = file_name)

# 加载grid_search_results
load("LH+LHCB+CPA+D0+H0+HCB0+CW_2025-02-09_14-39-42.RData")

# 查看结果 加载完.RData文件后从这里开始运行。
print(grid_search_results)
grid_search_results[!is.na(grid_search_results$avg_rmse) & !is.na(grid_search_results$avg_r_squared), ]

# 筛选最佳超参数组合
modela.nn.best <- grid_search_results[which.min(grid_search_results$avg_rmse), ]
print(modela.nn.best)

# 模型构建与性能评估
con <- file("test.log")
sink(con, append=TRUE)
sink(con, append=TRUE, type="message")

tryCatch({
  set.seed(123)
  modela.nn <- neuralnet(AGB ~ LH + LHCB + CPA + D0 + H0 + HCB0 + CW, 
                         data = picea.train, 
                         hidden = as.numeric(unlist(strsplit(modela.nn.best$hidden, ","))), 
                         linear.output = TRUE, 
                         err.fct = "sse", 
                         act.fct = "logistic", 
                         threshold = as.numeric(modela.nn.best$threshold), 
                         learningrate = as.numeric(modela.nn.best$learningrate), 
                         stepmax = as.numeric(modela.nn.best$stepmax),
                         lifesign = "full",
                         lifesign.step = 1)
}, error = function(e) {
  message("Error occurred during neuralnet execution: ", e$message)
}, finally = {
  sink()
  sink(type="message")
})

# 预测和评估
y.pred <- predict(modela.nn, picea.test)
FittingEvaluationIndex(y.pred, picea.test$AGB)


# 绘制残差图
data.nn <- data.frame(x = y.pred, y = y.pred - picea.test$AGB) 
p.nn <- ggplot(data.nn, aes(x = x, y = y)) +
  theme_light() +
  geom_point(color = "steelblue", size = 3, show.legend = F) +
  geom_hline(yintercept = c(0)) +
  geom_vline(xintercept = c(0)) +
  scale_x_continuous(limits = c(0, 0.6)) +
  scale_y_continuous(limits = c(-0.025, 0.025)) +
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
pdf("图13.15a.pdf", width = 8, height = 6, family = "GB1")
p.nn
dev.off()

# 绘制变化曲线
training_log <- readLines(con)
thresh <- stringr::str_extract(training_log, "min thresh: [0-9\\.]+")
thresh <- as.numeric(stringr::str_replace(thresh, "min thresh: ", ""))
iteration <- seq_along(thresh)
df <- data.frame(Iteration = iteration, MinThresh = thresh)
pdf("图13.15b.pdf", width = 8, height = 6, family = "GB1")
ggplot(df, aes(x = Iteration, y = MinThresh)) +
  theme_light() + 
  geom_line(color = "black") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 26, color = "black"),  # x轴标题字体大小
    axis.title.y = element_text(size = 26, color = "black"),  # y轴标题字体大小
    axis.text.x = element_text(size = 26, color = "black"),   # x轴文本字体大小
    axis.text.y = element_text(size = 26, color = "black"),   # y轴文本字体大小
    plot.title = element_text(size = 26, color = "black"),     # 图表标题字体大小
    legend.title = element_text(size = 26, color = "black"),   # 图例标题字体大小
    legend.text = element_text(size = 26, color = "black"),      # 图例文本字体大小
  )+
  xlim(0, 200) +        # 设置x轴显示范围
  ylim(-5, 45)           # 设置y轴显示范围

dev.off()

