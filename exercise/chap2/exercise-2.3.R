# 创建矩阵
mat1 <- matrix(1:6, nrow = 2)

# 矩阵转置
mat_t <- t(mat1)

# 矩阵逐元素乘法
mat_mul <- mat1 * mat1

# 创建数组
arr <- array(data = 1:12, dim = c(2, 3, 2))

# 数组访问与加法
slice1 <- arr[, , 1]
slice2 <- arr[, , 2]
arr_sum <- slice1 + slice2

# 输出结果
print(mat1)
print(mat_t)
print(mat_mul)
print(slice1)
print(slice2)
print(arr_sum)
