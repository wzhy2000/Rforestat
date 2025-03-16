# 加载必要的包
library(foreach)
library(doParallel)
library(neuralnet)
library(forestat)

# 数据准备
data("crassifolia")
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

# 设置并行计算的核心数
num_cores <- 2  # detectCores()
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# 定义网格搜索的函数，加入 tryCatch
grid_search_neuralnet <- function(train_data, test_data, tune_grid) {
  results <- foreach(params = iter(tune_grid, by = "row"), .combine = rbind, .packages = c("neuralnet")) %dopar% {
    result <- tryCatch({
      # 训练模型
      nn_model <- neuralnet(
        AGB ~ LH+LHCB+CPA+D0+H0+HCB0+CW, # LH+LHCB+CPA+D0+H0+HCB0+CW  需要修改
        data = train_data,
        hidden = unlist(params["hidden"]),
        linear.output = TRUE,
        err.fct = "sse",
        act.fct = "logistic",
        threshold = as.numeric(params["threshold"]),
        learningrate = as.numeric(params["learningrate"]),
        stepmax = as.numeric(params["stepmax"])
      )
      
      # 预测
      predictions <- predict(nn_model, test_data)
      
      # 计算性能指标
      data.frame(
        hidden = paste(unlist(params["hidden"]), collapse = ","),
        learningrate = as.numeric(params["learningrate"]),
        threshold = as.numeric(params["threshold"]),
        stepmax = as.numeric(params["stepmax"]),
        rmse = sqrt(mean((predictions - test_data$AGB)^2)),
        r_squared = cor(predictions, test_data$AGB)^2
      )
    }, error = function(e) {
      # 如果训练或预测过程中发生错误，返回 NA 或空的结果
      return(data.frame(
        hidden = paste(unlist(params["hidden"]), collapse = ","),
        learningrate = as.numeric(params["learningrate"]),
        threshold = as.numeric(params["threshold"]),
        stepmax = as.numeric(params["stepmax"]),
        rmse = NA,
        r_squared = NA
      ))
    })
    
    return(result)
  }
  
  return(results)
}

# 定义超参数网格  # 需要修改
tune_grid <- expand.grid(
  hidden = list(c(10, 10)),
  learningrate = seq(0.001, 0.1, by = 0.001),
  threshold = seq(0.01, 0.1, by = 0.01),
  stepmax = 200000
)

# 执行网格搜索并行计算
search_results <- grid_search_neuralnet(train_data, test_data, tune_grid)
current_time <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
file_name <- paste0("LH+LHCB+CPA+D0+H0+HCB0+CW_", current_time, ".RData")  # 需要修改
save(search_results, file = file_name)
# 找到最佳模型
best_model <- search_results[which.min(search_results$rmse), ]
print(best_model)

# 关闭并行计算
stopCluster(cl)
