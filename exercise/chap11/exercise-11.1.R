# 加载必要包
library(nlme)
library(dplyr)
library(fortedata)

# 加载土壤呼吸与处理数据
data <- fd_soil_respiration()
data <- na.omit(data)
data$log_efflux <- log1p(data$soil_co2_efflux)
###划分训练集和验证集
set.seed(1)
train_index <- createDataPartition(data$plot, p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# 设置初始值
start_vals <- c(a =1, b = 1, c = 0.1, d=0.1)

# 拟合 NLME 模型，a 为随机效应
nlme_model <- nlme(
  log_efflux ~ a / (1 + b * exp(-c * soil_temp - d * vwc)),
  data = train_data,
  fixed = a + b + c + d ~ 1,
  random = a~ 1 | replicate,
  start = start_vals,
  control = nlmeControl(maxIter = 200, pnlsTol = 1e-6)
)

# 查看结果
summary(nlme_model)

# 拟合普通非线性模型
nls_model <- nls(
  log_efflux ~ a / (1 + b * exp(-c * soil_temp - d * vwc)),
  data = train_data,
  start = start_vals
)

# 比较拟合优度
y_true <- test_data$log_efflux
y_prednlme <- predict(nlme_model,newdata = test_data)
y_prednls <- predict(nls_model,newdata = test_data)
# 计算 R²
r2nlme <- 1 - sum((y_true - y_prednlme)^2) / sum((y_true - mean(y_true))^2)
print(paste("R-squared:", round(r2nlme, 4)))
r2nls <- 1 - sum((y_true - y_prednls)^2) / sum((y_true - mean(y_true))^2)
print(paste("R-squared:", round(r2nls, 4)))

AIC(nlme_model)
AIC(nls_model)

# 残差标准差对比
sigma(nlme_model)
summary(nls_model)$sigma

# 提取残差与拟合值
res <- resid(nlme_model, type = "normalized")
fitted_vals <- fitted(nlme_model)

# 残差图
plot(fitted_vals, res,
     main = "Residual Plot (NLME)",
     xlab = "Fitted Values",
     ylab = "Normalized Residuals")
abline(h = 0, col = "red")
