# 习题 5.1：UCI Covertype 数据的主成分分析

# 数据来源：UCI Machine Learning Repository, Covertype (id = 31)。
# 优先使用 COVTYPE_FILE 环境变量或当前目录文件；缺失时从 UCI 官方下载。
locate_covertype <- function() {
  candidates <- c(
    Sys.getenv("COVTYPE_FILE", unset = ""),
    "covtype.data.gz",
    "covertype.zip"
  )
  candidates <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (length(candidates) > 0L) return(candidates[[1]])

  options(timeout = max(600, getOption("timeout")))
  destination <- file.path(tempdir(), "covertype.zip")
  message("未找到本地 Covertype 文件，正在从 UCI 官方下载约 11 MB 数据……")
  download.file(
    "https://archive.ics.uci.edu/static/public/31/covertype.zip",
    destination,
    mode = "wb",
    quiet = FALSE
  )
  destination
}

read_covertype <- function(path) {
  if (grepl("\\.zip$", path, ignore.case = TRUE)) {
    listing <- unzip(path, list = TRUE)
    member <- listing$Name[grepl("covtype\\.data(\\.gz)?$", listing$Name, ignore.case = TRUE)][1]
    if (is.na(member)) stop("UCI ZIP 中未找到 covtype.data 或 covtype.data.gz。")
    extracted <- unzip(path, files = member, exdir = tempdir(), overwrite = TRUE)
    if (grepl("\\.gz$", extracted, ignore.case = TRUE)) {
      read.csv(gzfile(extracted), header = FALSE)
    } else {
      read.csv(extracted, header = FALSE)
    }
  } else if (grepl("\\.gz$", path, ignore.case = TRUE)) {
    read.csv(gzfile(path), header = FALSE)
  } else {
    read.csv(path, header = FALSE)
  }
}

column_names <- c(
  "Elevation", "Aspect", "Slope",
  "Horizontal_Distance_To_Hydrology", "Vertical_Distance_To_Hydrology",
  "Horizontal_Distance_To_Roadways", "Hillshade_9am", "Hillshade_Noon",
  "Hillshade_3pm", "Horizontal_Distance_To_Fire_Points",
  paste0("Wilderness_Area_", 1:4), paste0("Soil_Type_", 1:40), "Cover_Type"
)

covtype <- read_covertype(locate_covertype())
stopifnot(ncol(covtype) == length(column_names))
names(covtype) <- column_names

# （1）只选六个连续且非方向性的变量。Aspect 是 0/360 度首尾相接的圆周变量，
# 若需要使用，应编码为 sin/cos；本题按要求排除。
vars <- c(
  "Elevation", "Slope", "Horizontal_Distance_To_Hydrology",
  "Vertical_Distance_To_Hydrology", "Horizontal_Distance_To_Roadways",
  "Horizontal_Distance_To_Fire_Points"
)
analysis_data <- covtype[, vars]
stopifnot(nrow(analysis_data) == 581012L, !anyNA(analysis_data))
cat("分析样本数：", nrow(analysis_data), "；变量数：", ncol(analysis_data), "\n")
cat("Aspect 为圆周变量，本题不把它当作普通线性变量纳入 PCA。\n")

# （2）比较标准化前后分布，并在 z-score 标准化数据上拟合 PCA。
print(summary(analysis_data))
X <- scale(analysis_data)
scaled_check <- rbind(mean = colMeans(X), sd = apply(X, 2, sd))
print(round(scaled_check, 6))
stopifnot(max(abs(colMeans(X))) < 1e-10, max(abs(apply(X, 2, sd) - 1)) < 1e-10)

pc <- prcomp(X, center = FALSE, scale. = FALSE)
variance_ratio <- pc$sdev^2 / sum(pc$sdev^2)
cumulative_ratio <- cumsum(variance_ratio)

# （3）输出 PC1、PC2 载荷；载荷整体符号翻转不改变解释。
print(round(pc$rotation[, 1:2, drop = FALSE], 4))
cat("载荷符号可能整体翻转；应根据绝对大小和变量方向解释环境梯度。\n")

# （4）输出解释率和达到预设 80% 所需的主成分数。
variance_table <- data.frame(
  component = paste0("PC", seq_along(variance_ratio)),
  explained = variance_ratio,
  cumulative = cumulative_ratio
)
print(variance_table)
n_for_80 <- which(cumulative_ratio >= 0.80)[1]
cat("累计解释率达到 80% 需要前 ", n_for_80, " 个主成分。\n", sep = "")

# （4）（5）PCA 用全数据拟合；仅为保证图形可读而固定种子抽取 2 000 个得分点。
set.seed(123)
plot_index <- sort(sample(seq_len(nrow(pc$x)), min(2000L, nrow(pc$x))))
scores <- pc$x[plot_index, 1:2, drop = FALSE]
loadings <- pc$rotation[, 1:2, drop = FALSE]

if (!interactive()) grDevices::cairo_pdf(tempfile("exercise-5.1-", fileext = ".pdf"), width = 9, height = 6)
par(mfrow = c(1, 2))
plot(
  seq_along(variance_ratio), variance_ratio,
  type = "b", pch = 19, xlab = "主成分", ylab = "解释率",
  main = "碎石图与累计解释率"
)
lines(seq_along(cumulative_ratio), cumulative_ratio, type = "b", pch = 17, col = "steelblue")
abline(h = 0.80, lty = 2, col = "red")
legend("right", c("单个解释率", "累计解释率"), col = c("black", "steelblue"), pch = c(19, 17), lty = 1)

plot(scores, pch = 16, cex = 0.35, col = rgb(0, 0, 0, 0.2), xlab = "PC1", ylab = "PC2", main = "PCA 双标图（得分抽样）")
arrow_scale <- 0.8 * min(diff(range(scores[, 1])), diff(range(scores[, 2])))
arrows(0, 0, loadings[, 1] * arrow_scale, loadings[, 2] * arrow_scale, col = "red", length = 0.08)
text(loadings[, 1] * arrow_scale, loadings[, 2] * arrow_scale, labels = rownames(loadings), col = "red", cex = 0.65, pos = 3)
if (!interactive()) dev.off()
cat("双标图描述变量方向与样本得分的几何关系，不能据此作因果判断。\n")
