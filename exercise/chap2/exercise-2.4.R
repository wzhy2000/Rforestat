# 习题 2.4：成绩等级判定函数

# （1）定义函数，并对输入范围和边界值进行检查。
grade_evaluation <- function(score) {
  stopifnot(is.numeric(score), length(score) == 1L, !is.na(score))
  stopifnot(score >= 0, score <= 100)

  if (score >= 90) {
    "优秀"
  } else if (score >= 75) {
    "良好"
  } else if (score >= 60) {
    "及格"
  } else {
    "不及格"
  }
}

scores <- c(59, 60, 74, 75, 89, 90, 100)
expected <- c("不及格", "及格", "及格", "良好", "良好", "优秀", "优秀")
actual <- vapply(scores, grade_evaluation, character(1))
stopifnot(identical(unname(actual), expected))

# （2）用 for 循环逐个输出成绩及其等级。
results <- data.frame(score = scores, grade = character(length(scores)))
for (i in seq_along(scores)) {
  results$grade[i] <- grade_evaluation(scores[i])
  cat("成绩：", scores[i], "；等级：", results$grade[i], "\n", sep = "")
}
print(results)
