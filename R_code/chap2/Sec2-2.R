# 2.2 数据结构

# 2.2.1 向量
# 向量创建
num_vector_a <- c(1, 2, 3, 4, 5)
num_vector_b <- c(2, 3, 4, 5, 6)
char_vector <- c("one", "two", "three")
logical_vector <- c(TRUE, FALSE, TRUE)
rep_vector <- rep(0, times=5)
print(rep_vector)
seq_vector <- seq(from=1, to=10, by=2)
print(seq_vector)

# 向量操作
num_vector_a[3]
char_vector[c(1, 3)]
seq_vector[2] <- 11
print(seq_vector)

# 向量运算
num_vector_a + 2
num_vector_a
sum_vector <- num_vector_a + num_vector_b
sum_vector
num_vector_b <- num_vector_b * 2
num_vector_b
prod_vector <- num_vector_a * num_vector_b
prod_vector

# 向量化函数
sqrt_vector <- sqrt(prod_vector)
sqrt_vector
max_value <- max(prod_vector)
max_value

# 逻辑向量和字符向量
greater_than_5 <- sqrt_vector > 5
print(greater_than_5)
element_char_vector <- paste(char_vector, "element", sep="_")
print(element_char_vector)

# 2.2.2 矩阵
# 创建矩阵
m <- matrix(1:9, nrow=3, ncol=3)
print(m)
m_byrow <- matrix(1:9, nrow=3, byrow=TRUE)
print(m_byrow)
m_rbind <- rbind(1:3, 4:6, 7:9)
print(m_rbind)
m_cbind <- cbind(1:3, 3:1, -1:-3)
print(m_cbind)

# 矩阵操作
m[2, 3]
second_row <- m[2, ]
print(second_row)
third_column <- m[, 3]
print(third_column)
m[1, 1] <- 10
print(m)

# 矩阵运算
m_rbind + m_cbind
m_rbind - m_cbind
m_rbind * m_cbind
m_rbind / m_cbind
m_rbind + m_cbind
m_rbind %*% m_cbind
t(m_byrow)
solve(m_rbind * m_cbind)

# 2.2.3 数组
# 创建数组
array_3d <- array(data = 1:18, dim = c(2, 3, 3))
print(array_3d)

# 数组操作
element <- array_3d[1, 2, 3]
print(element)
row1 <- array_3d[1, , ]
print(row1)
col2 <- array_3d[, 2, ]
print(col2)

# 数组运算
array1 <- array(data = 1:18, dim = c(2, 3, 3))
array2 <- array(data = c(1:9, -9:-1), dim = c(2, 3, 3))
sum_array <- array1 + array2
print(sum_array)

# 数组与矩阵的转换
matrix <- matrix(1:6, nrow = 2, ncol = 3)
class(matrix)
array_2d <- array(data = 1:6, dim = c(2, 3))
class(array_2d)
matrix * array_2d
matrix(array1, nrow = 2, ncol = 9)
array(matrix, dim = c(3, 2))

# 2.2.4 列表
# 创建列表
person_list <- list(name="John Doe", age=25, scores=c(90, 80, 85), is_student=TRUE)
print(person_list)

# 访问列表元素
person_list[[1]]
person_list[2:3]
person_list$scores
person_list$scores[1]

# 修改列表
person_list[[2]] <- 26
person_list$gender <- "male"
person_list$is_student <- NULL
print(person_list)

# 列表嵌套与合并
nested_list <- list(
  personal_info=list(name="Alice", age=30),
  scores=c(95, 85, 90)
)
nested_list$personal_info <- c(nested_list$personal_info, gender="male")
print(nested_list)

# 2.2.5 数据框
# 创建数据框
class_df <- data.frame(
  Name = c("Alice", "Bob", "Charlie"),
  Age = c(24, 26, 28),
  Score = c(88, 90, 85)
)
head(class_df)
str(class_df)
summary(class_df)

# 访问数据框的元素
class_df$Name
class_df[["Age"]]
class_df[1]
class_df["Age"]
class_df[1, ]
class_df[, 1]
class_df[2, "Score"]
class_df[3, 3]

# 修改数据框
names(class_df)[1] <- "FirstName"
rownames(class_df)[1] <- "Person1"
class_df$Gender <- c("Female", "Male", "Male")
new_row <- data.frame(
  FirstName = "David",
  Age = 25,
  Gender = "Male",
  Score = 96
)
class_df <- rbind(class_df, new_row)
class_df$Age <- NULL
class_df <- class_df[-3:-4, ]
class_df <- class_df[class_df$Score >= 90, ]
print(class_df)

# 2.2.6 因子
# 创建因子
gender <- factor(c("male", "female", "female", "male"))
print(gender)

# 创建有序因子
rating <- factor(c("low", "medium", "high", "medium"), ordered = TRUE)
print(rating)

# 因子的水平
levels(rating)
rating <- factor(rating, levels = c("low", "medium", "high"))
print(rating)
levels(rating) <- c("small", "medium", "large")
print(rating)

# 有序因子的比较
rating > "small"
sorted_rating <- sort(rating)
print(sorted_rating)

# 因子与字符向量的转换
vec <- c("a", "b", "c", "a")
fac <- as.factor(vec)
print(fac)
char_vec <- as.character(fac)
print(char_vec)






