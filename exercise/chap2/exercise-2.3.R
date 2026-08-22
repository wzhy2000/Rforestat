# 习题 2.3：矩阵和三维数组

# （1）创建 2 x 3 矩阵，转置并核对维度。
m <- matrix(1:6, nrow = 2, byrow = TRUE)
m_transposed <- t(m)
print(m)
print(m_transposed)
print(rbind(original = dim(m), transposed = dim(m_transposed)))
stopifnot(identical(dim(m), c(2L, 3L)), identical(dim(m_transposed), c(3L, 2L)))

# （2）逐元素乘法仍为 2 x 3；矩阵乘法 m %*% t(m) 为 2 x 2。
elementwise_product <- m * m
matrix_product <- m %*% t(m)
print(elementwise_product)
print(matrix_product)
stopifnot(identical(dim(elementwise_product), c(2L, 3L)))
stopifnot(identical(dim(matrix_product), c(2L, 2L)))

# （3）创建两个同维数组，访问切片并逐元素相加。
a1 <- array(1:12, dim = c(2, 3, 2))
a2 <- array(12:1, dim = c(2, 3, 2))
first_slice <- a1[, , 1]
array_sum <- a1 + a2
print(first_slice)
print(array_sum)
stopifnot(all(array_sum == 13))
