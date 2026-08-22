# 习题 2.2：数值向量、逻辑比较与索引

# （1）创建向量并计算描述统计量。
x <- c(2, 5, 7, 9, 1, 5, 6, 4)
statistics <- c(mean = mean(x), variance = var(x), sd = sd(x))
print(statistics)

# （2）严格以 5 为阈值；等于 5 的元素不会被选中。
greater_than_5 <- x > 5
print(greater_than_5)
stopifnot(!any(greater_than_5[x == 5]))

# （3）同时输出满足条件的位置和原始数值。
indices <- which(greater_than_5)
selected <- data.frame(index = indices, value = x[indices])
print(selected)
