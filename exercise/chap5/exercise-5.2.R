# 习题 5.2：mtcars 标准化与距离矩阵比较

vars <- c("mpg", "disp", "hp", "drat", "wt", "qsec")
analysis_data <- datasets::mtcars[, vars]

# （1）核对量纲和异常值，并对全部分析变量进行 z-score 标准化。
print(summary(analysis_data))
outlier_counts <- vapply(analysis_data, function(x) {
  limits <- quantile(x, c(0.25, 0.75)) + c(-1.5, 1.5) * IQR(x)
  sum(x < limits[1] | x > limits[2])
}, integer(1))
print(outlier_counts)
X <- scale(analysis_data)
print(round(rbind(mean = colMeans(X), sd = apply(X, 2, sd)), 6))

# （2）分别构造欧氏距离和曼哈顿距离矩阵。
euclidean <- dist(X, method = "euclidean")
manhattan <- dist(X, method = "manhattan")
stopifnot(
  identical(attr(euclidean, "Size"), 32L),
  identical(attr(manhattan, "Size"), 32L)
)
distance_summary <- cbind(
  euclidean = summary(as.vector(euclidean)),
  manhattan = summary(as.vector(manhattan))
)
print(distance_summary)
cat("两种距离向量的相关系数：", cor(as.vector(euclidean), as.vector(manhattan)), "\n")

# （3）比较距离分布、集中程度和极端距离。
distribution_stats <- rbind(
  euclidean = c(mean = mean(euclidean), sd = sd(euclidean), max = max(euclidean)),
  manhattan = c(mean = mean(manhattan), sd = sd(manhattan), max = max(manhattan))
)
print(distribution_stats)

distance_values <- c(as.vector(euclidean), as.vector(manhattan))
plot_xlim <- c(0, max(pretty(c(0, max(distance_values)))))
n_breaks <- max(10L, nclass.FD(distance_values))
common_breaks <- seq(plot_xlim[1], plot_xlim[2], length.out = n_breaks + 1L)
euclidean_histogram <- hist(as.vector(euclidean), breaks = common_breaks, plot = FALSE)
manhattan_histogram <- hist(as.vector(manhattan), breaks = common_breaks, plot = FALSE)
plot_ylim <- c(0, max(euclidean_histogram$counts, manhattan_histogram$counts) * 1.08)

if (!interactive()) grDevices::cairo_pdf(tempfile("exercise-5.2-", fileext = ".pdf"), width = 8, height = 5)
plot(
  euclidean_histogram,
  col = rgb(0.2, 0.4, 0.8, 0.5), border = "steelblue",
  xlim = plot_xlim, ylim = plot_ylim, xaxs = "i",
  xlab = "距离", main = "欧氏与曼哈顿距离分布"
)
plot(
  manhattan_histogram,
  col = rgb(0.9, 0.4, 0.2, 0.35), border = "firebrick",
  add = TRUE
)
abline(v = max(euclidean), col = "steelblue", lty = 2)
abline(v = max(manhattan), col = "firebrick", lty = 2)
legend(
  "topright", c("欧氏距离", "曼哈顿距离", "各自最大值"),
  fill = c(rgb(0.2, 0.4, 0.8, 0.5), rgb(0.9, 0.4, 0.2, 0.35), NA),
  border = c("steelblue", "firebrick", NA),
  lty = c(NA, NA, 2), col = c(NA, NA, "gray30"), bty = "n"
)
if (!interactive()) dev.off()

# （4）量纲会改变变量权重，距离定义会改变大偏差的相对影响。
cat("未标准化时大尺度变量会主导距离；欧氏距离更强调大偏差，曼哈顿距离线性累加各维差异。\n")
