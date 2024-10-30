data=read.csv('data10-1.csv')
colname = c('CW','D','L','CLR','Pt')#选择需要的列

# 构建混合效应模型 CW_model
CW_model = nlme(
  CW ~ (phi1 + phi2 * CLR) / (1 + phi3 * exp(-(phi4 + phi5 * L) * D)), # 设置模型公式
  data = data,                                                         # 指定数据集
  fixed = (phi1 + phi2 + phi3 + phi4 + phi5 ~ 1),                      # 设置固定效应项
  random = list(Pt = phi1 ~ 1),                                        # 设置混合效应结构 (随机效应)
  start = c(phi1 = 4.396, phi2 = 1.840, phi3 = 2.231, phi4 = 0.1356, phi5 = -0.00002093) # 初始值
)

# 显示模型汇总信息
summary(CW_model)

# 提取混合效应的随机效应
CW_ranef = ranef(CW_model)

# 查看随机效应的前几行数据
head(CW_ranef)

# 使用模型预测 CW 值
data$CW_fit <- predict(CW_model)

# 计算模型的 R 平方值
sst <- sum((data$CW - mean(data$CW))^2)    # 总平方和
ssr <- sum((data$CW_fit - mean(data$CW))^2) # 回归平方和
r_squared <- ssr / sst                      # R 平方值

# 绘制预测值和观测值的散点图
plot(data$CW_fit, data$CW, xlab = "predicted", ylab = "observed", col = "black")

# 绘制模型的拟合值与残差图
plot(fitted(CW_model), residuals(CW_model), main = "nlme Residuals")

# 使用 nls 函数构建非线性回归模型 CW_nls
CW_nls = nls(
  CW ~ (phi1 + phi2 * CLR) / (1 + phi3 * exp(-(phi4 + phi5 * L) * D)), # 设置公式
  data = data,                                                         # 指定数据
  start = c(phi1 = 4.396, phi2 = 1.840, phi3 = 2.231, phi4 = 0.1356, phi5 = -0.00002093) # 初始值
)

# 显示 nls 模型的汇总信息
summary(CW_nls)

# 预测 nls 模型的 CW 值
predict_nls = predict(CW_nls)

# 计算 nls 模型的 R 平方值
nls_sst <- sum((data$CW - mean(data$CW))^2)    # 总平方和
nls_ssr <- sum((predict_nls - mean(data$CW))^2) # 回归平方和
nls_r_squared <- nls_ssr / nls_sst              # R 平方值

# 通过方差分析对 nlme 模型和 nls 模型进行比较
anova(CW_model, CW_nls)