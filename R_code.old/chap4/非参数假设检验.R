################ 1  使用皮尔逊chi^2检验白桦树树高是否符合均值为10、标准差为4的正态分布 ################

library(forestat)
data("birch")

# 1. 准备数据
H <- birch$H  # 提取白桦树树高

# 2. 分箱处理
breaks <- seq(min(H), max(H) + 1, by = 1)  # 分箱，区间宽度为1
obs <- table(cut(H, breaks, right = FALSE))  # 观测频数
obs <- as.numeric(obs)  # 转换为向量

# 3. 计算理论概率值
mu <- 10  # 指定均值
sigma <- 4  # 指定标准差
theoretical_probs <- pnorm(breaks[-1], mean = mu, sd = sigma) - 
  pnorm(breaks[-length(breaks)], mean = mu, sd = sigma)

# 4. 合并低频箱
print(obs)
obs_combined <- c(obs[1:19], sum(obs[20:length(obs)]))  # 合并低频区
theoretical_probs_combined <- c(theoretical_probs[1:19], sum(theoretical_probs[20:length(theoretical_probs)]))

# 5. 标准化理论概率（若未归一化）
theoretical_combined <- theoretical_probs_combined / sum(theoretical_probs_combined)


# 6. 卡方检验
chisq_result1 <- chisq.test(obs_combined, p = theoretical_combined)

# 显示结果
print(chisq_result1)

####################### 2 使用皮尔逊chi^2\)检验白桦树树高是否符合正态分布（参数未知） ###################

H <- birch$H  # 提取白桦树树高

# 2. 分箱处理
breaks <- seq(min(H), max(H) + 1, by = 1)  # 分箱，区间宽度为1
obs <- table(cut(H, breaks, right = FALSE))  # 观测频数
obs <- as.numeric(obs)  # 转换为向量

# 3. 计算理论频数
mu <- mean(H)  # 指定均值
sigma <- sd(H)  # 指定标准差
theoretical_probs <- pnorm(breaks[-1], mean = mu, sd = sigma) - 
  pnorm(breaks[-length(breaks)], mean = mu, sd = sigma)

# 4. 合并低频箱
obs_combined <- c(obs[1:19], sum(obs[20:length(obs)]))  # 合并低频区
theoretical_probs_combined <- c(theoretical_probs[1:19], sum(theoretical_probs[20:length(theoretical_probs)]))

# 5. 标准化理论概率（若未归一化）
theoretical_combined <- theoretical_probs_combined / sum(theoretical_probs_combined)


# 6. 卡方检验
chisq_result2 <- chisq.test(obs_combined, p = theoretical_combined)


df <- chisq_result2$parameter - 2
Pval <- 1 - pchisq(chisq_result2$statistic, df)
cat("Chi-squared statistic:", chisq_result2$statistic, "\n",
    "P-value:", Pval, "\n",
    "Degrees of freedom:", df, "\n")

####################### 3 使用K-S检验白桦树树高是否符合正态分布 ###################

# 提取白桦树的树高数据
H <- birch$H

# 计算样本的均值和标准差（假定正态分布的参数）
mu <- mean(H)
sigma <- sd(H)

set.seed(123)
# Kolmogorov-Smirnov 检验假设需要样本来自一个连续分布。如果样本数据中存在重复值，可能会报错。
# 在不改变数据总体分布的前提下，可以对数据加入少量随机噪声，使其成为“近似连续”的数据。
H_perturbed <- H + rnorm(length(H), mean = 0, sd = 1e-6)  
# 使用 K-S 检验
ks_result <- ks.test(H_perturbed, "pnorm", mean = mu, sd = sigma)

# 显示检验结果
print(ks_result)


################### 4 使用K-S检验白桦树树高和落叶松树高的分布是否一致  #####################
data("larch")

# 提取白桦树和落叶松的树高
H_birch <- birch$H
H_larch <- larch$H

# 添加很小的随机噪声
H_birch <- H_birch + runif(length(H_birch), min = -1e-6, max = 1e-6)
H_larch <- H_larch + runif(length(H_larch), min = -1e-6, max = 1e-6)

# 执行 Kolmogorov-Smirnov 检验
ks_result <- ks.test(H_birch, H_larch)

# 输出结果
print(ks_result)



############# 5 使用Shapiro-Wilk检验白桦树树高是否符合正态分布 ################

# 提取白桦树的树高数据
H_birch <- birch$H

# Shapiro-Wilk 正态性检验
shapiro_result <- shapiro.test(H_birch)

# 显示结果
print(shapiro_result)








