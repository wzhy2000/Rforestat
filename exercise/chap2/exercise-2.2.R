# 创建向量
a <- c(2, 5, 8, 3, 9)

# 计算统计量
mean_a <- mean(a)
var_a <- var(a)
sd_a <- sd(a)

# 逻辑判断
greater_than_5 <- a > 5
indices <- which(greater_than_5)

# 奇偶判断
parity <- ifelse(a %% 2 == 0, "偶数", "奇数")

# 输出结果
print(mean_a)
print(var_a)
print(sd_a)
print(greater_than_5)
print(indices)
print(parity)
