# 习题 2.1：R 与 RStudio 的基本配置和简单绘图

# （1）记录当前 R 版本。RStudio 版本需在 RStudio 中实际核对。
cat("R 版本：", R.version.string, "\n")
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  cat("RStudio 版本：", as.character(rstudioapi::versionInfo()$version), "\n")
} else {
  cat("当前不是 RStudio 会话；请在 Help > About RStudio 中记录实际版本。\n")
}

# （2）RStudio 四个主要窗格：
# Source：编辑和保存脚本；Console：执行命令并显示结果；
# Environment/History：查看对象和命令历史；
# Files/Plots/Packages/Help：浏览文件、图形、软件包与帮助。

# （3）绘制正弦函数，并说明主要参数。
x <- seq(-2 * pi, 2 * pi, length.out = 400)
if (!interactive()) {
  grDevices::cairo_pdf(tempfile("exercise-2.1-", fileext = ".pdf"), width = 7, height = 5)
}
plot(
  x, sin(x),
  type = "l",       # 折线图
  lwd = 2,          # 线宽
  col = "steelblue", # 线条颜色
  xlab = "x",      # x 轴标题
  ylab = "sin(x)", # y 轴标题
  main = "正弦函数" # 图题
)
if (!interactive()) dev.off()
