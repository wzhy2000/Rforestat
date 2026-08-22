# 习题 2.5：可复现的随机模拟与图形导出

# （1）固定随机种子，生成 1 000 个标准正态随机数。
set.seed(123)
x <- rnorm(1000)
statistics <- c(mean = mean(x), sd = sd(x))
print(statistics)

# （2）（3）绘制直方图，并按指定像素尺寸和分辨率导出。
output_file <- "histogram.png"
width_px <- 1800
height_px <- 1200
resolution_dpi <- 200

png(output_file, width = width_px, height = height_px, res = resolution_dpi)
hist(
  x,
  breaks = "FD",
  col = "skyblue",
  border = "white",
  main = "标准正态随机数直方图",
  xlab = "模拟值",
  ylab = "频数"
)
dev.off()

stopifnot(file.exists(output_file), file.info(output_file)$size > 0)
cat(
  "图形文件：", output_file,
  "；宽度：", width_px, " px",
  "；高度：", height_px, " px",
  "；分辨率：", resolution_dpi, " dpi\n",
  sep = ""
)
