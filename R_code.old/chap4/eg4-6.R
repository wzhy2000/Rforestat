library("forestat")
data(larch)
data.1 <- larch[larch$PLOT == "28", c("D")]
data.2 <- larch[larch$PLOT == "71", c("D")]


# 在假设两块样地胸径方差相等的条件下，对胸径均值进行双侧假设检验
t.test(data.1, data.2, var.equal = TRUE)

# 在假设两块样地胸径方差不相等的情况下，采用 Welch \(t\) 检验对胸径均值进行双侧假设检验
t.test(data.1, data.2)

# 在方差不相等的前提下，对胸径均值进行单侧假设检验
t.test(data.1, data.2, alternative = "less")

# 对两块样地胸径方差的比值进行假设检验
var.test(data.1, data.2)

# 采用配对检验方法，检验两块样地落叶松胸径平均值是否存在显著差异
t.test(data.1, data.2, paired = TRUE)
