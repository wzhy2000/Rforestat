# 定义函数
grade_evaluation <- function(score) {
  if (score >= 90) {
    return("优秀")
  } else if (score >= 75) {
    return("良好")
  } else if (score >= 60) {
    return("及格")
  } else {
    return("不及格")
  }
}

# 成绩向量
scores <- c(92, 67, 80, 55)

# 遍历并输出
for (s in scores) {
  cat("成绩:", s, "等级:", grade_evaluation(s), "\n")
}
