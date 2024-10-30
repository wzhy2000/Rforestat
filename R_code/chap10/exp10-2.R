
data = read.csv("data10-2.csv")

# 使用 nlme 构建混合效应模型 height_model
library(nlme)

height_model = nlme(
  model = H ~ phi1 + phi2 * exp(-phi3 / A),  # 模型公式
  data = data,                               # 数据集
  fixed = list(phi2 ~ 1, phi3 ~ C),          # 设置 phi2 和 phi3 的固定效应，其中 phi3 随 C 变化
  random = list(Bk = phi1 ~ 1),              # phi1 的随机效应项，按 Bk 分组
  start = c(phi2 = 15.087, phi3 = c(12.734, 12.734)) # 初始值
)

# 输出模型汇总信息
summary(height_model)

# 提取模型的随机效应
ranef(height_model)

# 使用 height_model 模型预测高度 H，并将预测结果存入数据框
data$H_fit = predict(height_model)

# 计算 R 平方值
sst <- sum((data$H - mean(data$H))^2)      # 总平方和
ssr <- sum((data$H_fit - mean(data$H))^2)  # 回归平方和
r_squared <- ssr / sst                     # R 平方

# 将多个变量组合成分组列，用于绘图中的分面
data$group = paste(data$Bk, data$Pt, data$Tree, sep = "_")

# 使用 ggplot2 绘制观测和拟合的高度曲线
library(ggplot2)
ggplot(data, aes(x = A)) + 
  geom_point(aes(y = H, shape = "Actual Height"), color = "black") +         # 实际高度的散点
  geom_line(aes(y = H, linetype = "Actual Height")) +                        # 实际高度的折线
  geom_point(aes(y = H_fit, shape = "Fitted Height"), color = "black") +     # 拟合高度的散点
  geom_line(aes(y = H_fit, linetype = "Fitted Height")) +                    # 拟合高度的折线
  facet_wrap(~group, ncol = 3) +                                             # 按组分面展示，列数为 3
  xlab("Age") + ylab("Height") +                                             # 设置坐标轴标签
  scale_shape_manual(values = c(16, 2)) +                                    # 自定义形状
  scale_linetype_manual(values = c("solid", "dashed"))                       # 自定义折线样式

# 构建非线性回归模型 height_nls
height_nls = nls(
  H ~ phi2 * exp(-phi3 / A),              # 模型公式
  data = data,                            # 数据集
  start = c(phi2 = 15.087, phi3 = 12.734) # 初始值
)

# 显示 nls 模型的汇总信息
summary(height_nls)

# 预测 nls 模型的高度 H，并计算 R 平方值
predict_nls = predict(height_nls)
nls_sst <- sum((data$H - mean(data$H))^2)      # 总平方和
nls_ssr <- sum((predict_nls - mean(data$H))^2) # 回归平方和
nls_r_squared <- nls_ssr / nls_sst             # R 平方

# 通过方差分析对比 height_model 和 height_nls 模型
anova(height_model, height_nls)

# 构建改进后的混合效应模型 height_model2
height_model2 = nlme(
  model = H ~ phi1 + phi2 * exp(-phi3 / A),  # 模型公式
  data = data,                               # 数据集
  fixed = list(phi2 ~ 1, phi3 ~ C),          # 设置 phi2 和 phi3 的固定效应，其中 phi3 随 C 变化
  random = list(Bk = phi1 ~ 1, Pt = phi1 ~ 1),# 按 Bk 和 Pt 分组的 phi1 随机效应
  start = c(phi2 = 15.087, phi3 = c(12.734, 12.734)) # 初始值
)

# 输出 height_model2 的汇总信息
summary(height_model2)

# 预测 height_model2 模型的高度 H，并计算 R 平方
predict_model2 = predict(height_model2)
model2_sst <- sum((data$H - mean(data$H))^2)       # 总平方和
model2_ssr <- sum((predict_model2 - mean(data$H))^2) # 回归平方和
model2_r_squared <- model2_ssr / model2_sst        # R 平方