# 习题 5.3：UCI Wine 数据的 k-means 聚类与外部验证

library(cluster)
library(mclust)

locate_wine <- function() {
  candidates <- c(Sys.getenv("WINE_FILE", unset = ""), "wine.data", "wine.zip")
  candidates <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (length(candidates) > 0L) return(candidates[[1]])

  options(timeout = max(300, getOption("timeout")))
  destination <- file.path(tempdir(), "wine.zip")
  download.file(
    "https://archive.ics.uci.edu/static/public/109/wine.zip",
    destination, mode = "wb", quiet = TRUE
  )
  destination
}

read_wine <- function(path) {
  if (grepl("\\.zip$", path, ignore.case = TRUE)) {
    listing <- unzip(path, list = TRUE)
    member <- listing$Name[grepl("(^|/)wine\\.data$", listing$Name, ignore.case = TRUE)][1]
    if (is.na(member)) stop("UCI ZIP 中未找到 wine.data。")
    path <- unzip(path, files = member, exdir = tempdir(), overwrite = TRUE)
  }
  read.csv(path, header = FALSE)
}

wine <- read_wine(locate_wine())
names(wine) <- c(
  "Class", "Alcohol", "Malic_acid", "Ash", "Alcalinity_of_ash", "Magnesium",
  "Total_phenols", "Flavanoids", "Nonflavanoid_phenols", "Proanthocyanins",
  "Color_intensity", "Hue", "OD280_OD315", "Proline"
)
stopifnot(nrow(wine) == 178L, ncol(wine) == 14L, !anyNA(wine))

# （1）真实类别只留作最终外部验证，不进入聚类或 k 的选择。
truth <- factor(wine$Class)
X <- scale(wine[, setdiff(names(wine), "Class")])

# （2）（3）比较 k=2,...,10 的肘部指标和平均轮廓系数。
set.seed(123)
ks <- 2:10
fits <- lapply(ks, function(k) kmeans(X, centers = k, nstart = 50))
wss <- vapply(fits, function(z) z$tot.withinss, numeric(1))
distance_matrix <- dist(X)
silhouette_mean <- mapply(
  function(z, k) mean(silhouette(z$cluster, distance_matrix)[, "sil_width"]),
  fits, ks
)

# （4）用重复 80% 子样本与全数据聚类的一致性衡量候选 k 的稳定性。
set.seed(123)
stability <- vapply(seq_along(ks), function(i) {
  mean(replicate(30, {
    index <- sample(seq_len(nrow(X)), floor(0.8 * nrow(X)))
    subsample_fit <- kmeans(X[index, , drop = FALSE], centers = ks[i], nstart = 30)
    adjustedRandIndex(fits[[i]]$cluster[index], subsample_fit$cluster)
  }))
}, numeric(1))

selection <- data.frame(k = ks, wss = wss, silhouette = silhouette_mean, stability = stability)
print(selection)
selected_k <- selection$k[which.max(selection$silhouette)]
cat("平均轮廓系数首选 k：", selected_k, "；应同时结合肘部、稳定性和研究目的判断。\n")

if (!interactive()) grDevices::cairo_pdf(tempfile("exercise-5.3-", fileext = ".pdf"), width = 9, height = 5)
par(mfrow = c(1, 2))
plot(ks, wss, type = "b", pch = 19, xlab = "k", ylab = "组内平方和", main = "肘部法")
plot(ks, silhouette_mean, type = "b", pch = 19, xlab = "k", ylab = "平均轮廓系数", main = "轮廓系数")
if (!interactive()) dev.off()

# （5）在无监督选择完成后，才用真实类别报告 ARI。
chosen_fit <- fits[[which(ks == selected_k)]]
ari <- adjustedRandIndex(chosen_fit$cluster, truth)
cat("选定 k 的外部验证 ARI：", ari, "\n")
cat("真实类别不参与聚类训练或 k 的选择，也不能被当作无监督聚类的必然答案。\n")
