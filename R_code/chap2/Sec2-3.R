# 2.3 编程结构

# 2.3.1 逻辑控制
# 循环语句
# for循环
numbers <- c(1, 2, 3, 4, 5)
sum_even <- 0
sum_odd <- 0
for (num in numbers) {
  if (num %% 2 == 0) {
    sum_even <- sum_even + num
  } else {
    sum_odd <- sum_odd + num
  }
}
cat("偶数的和:", sum_even, "\n")
cat("奇数的和:", sum_odd, "\n")

# while循环
count <- 1
max_count <- 5
while (count <= max_count) {
  cat("当前计数:", count, "\n")
  count <- count + 1
}

# repeat循环
count <- 1
repeat {
  cat("Repeat循环中的当前计数:", count, "\n")
  count <- count + 1
  if (count > max_count) {
    break
  }
}

# 条件判断函数
# ifelse()函数
results <- ifelse(numbers %% 2 == 0, "偶数", "奇数")
print(results)

# switch()函数
for (i in 1:length(numbers)) {
  result <- switch(as.character(numbers[i]),
                   "1" = "壹",
                   "2" = "贰",
                   "3" = "叁",
                   "4" = "肆",
                   "5" = "伍",
                   "其他" = "未知")
  cat("数字", numbers[i], "对应的中文大写:", result, "\n")
}

# 2.3.2 函数
# 定义函数
add_numbers <- function(a, b) {
  sum <- a + b
  return(sum)
}
result <- add_numbers(1, 2)
print(result)

# 参数传递
add_numbers <- function(a = 0, b = 0) {
  sum <- a + b
  return(sum)
}
result <- add_numbers(1)
print(result)

result <- add_numbers(b = 1, a = 2)
print(result)

# 函数的作用域
x <- 10
my_function <- function(y) {
  x <- 1
  return(x + y)
}
result <- my_function(2)
print(result)
print(x)

# 匿名函数
result <- (function(a, b) { return(a + b) })(1, 2)
print(result)
