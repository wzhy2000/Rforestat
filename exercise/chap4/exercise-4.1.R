# 习题 4.1：评估树高分布对正态分布的偏离

data("picea", package = "forestat")
x <- na.omit(picea$LH)

skewness <- function(z) {
  z <- z[is.finite(z)]
  mean((z - mean(z))^3) / stats::sd(z)^3
}

# （1）核对样本量、缺失值和正值条件，并绘制直方图与 Q-Q 图。
cat(
  "LH 有效样本量：", length(x),
  "；缺失数：", sum(is.na(picea$LH)),
  "；是否严格为正：", all(x > 0), "\n",
  sep = ""
)
cat("LH 的定义和单位应结合 help('picea', package='forestat') 核对。\n")

if (!interactive()) grDevices::cairo_pdf(tempfile("exercise-4.1-", fileext = ".pdf"), width = 9, height = 7)
par(mfrow = c(2, 2))
hist(x, main = "LH 原尺度直方图", xlab = "树高 LH", col = "skyblue", border = "white")
qqnorm(x, main = "LH 原尺度 Q-Q 图")
qqline(x, col = "red")

# （2）检验结果用于评估是否有明显偏离，不能“证明正态”。
original_test <- shapiro.test(x)
print(original_test)
cat("原尺度偏度：", skewness(x), "\n")

# （3）仅在严格为正时取对数，并比较偏度、图形和检验。
if (all(x > 0)) {
  log_x <- log(x)
  hist(log_x, main = "log(LH) 直方图", xlab = "log(LH)", col = "lightgreen", border = "white")
  qqnorm(log_x, main = "log(LH) Q-Q 图")
  qqline(log_x, col = "red")
  log_test <- shapiro.test(log_x)
  print(log_test)
  cat("对数尺度偏度：", skewness(log_x), "\n")
}
if (!interactive()) dev.off()
cat("p >= 0.05 仅表示当前数据未检出明显偏离，不能证明总体服从正态分布。\n")
