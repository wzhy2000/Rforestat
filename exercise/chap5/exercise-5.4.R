# 习题 5.4：森林样地环境数据的 Q 型与 R 型聚类

library(cluster)

# （1）使用明确的模拟机制构造 120 个样地；坡向用 sin/cos 编码。
set.seed(123)
n_each <- 40L
latent_type <- factor(rep(c("低海拔", "中海拔", "高海拔"), each = n_each))
environment <- data.frame(
  elevation_m = c(rnorm(n_each, 550, 90), rnorm(n_each, 1000, 100), rnorm(n_each, 1500, 110)),
  slope_degree = c(rnorm(n_each, 9, 3), rnorm(n_each, 19, 4), rnorm(n_each, 29, 4)),
  aspect_degree = runif(3 * n_each, 0, 360),
  soil_pH = c(rnorm(n_each, 6.5, 0.25), rnorm(n_each, 5.8, 0.25), rnorm(n_each, 5.2, 0.25)),
  mean_temp_c = c(rnorm(n_each, 15, 1), rnorm(n_each, 10, 1), rnorm(n_each, 5, 1)),
  annual_precip_mm = c(rnorm(n_each, 720, 60), rnorm(n_each, 930, 70), rnorm(n_each, 1120, 80))
)
environment$aspect_sin <- sin(environment$aspect_degree * pi / 180)
environment$aspect_cos <- cos(environment$aspect_degree * pi / 180)

features <- environment[, c(
  "elevation_m", "slope_degree", "soil_pH", "mean_temp_c", "annual_precip_mm",
  "aspect_sin", "aspect_cos"
)]
stopifnot(!anyNA(features), all(vapply(features, is.numeric, logical(1))))
X <- scale(features)
print(round(rbind(mean = colMeans(X), sd = apply(X, 2, sd)), 6))

# （2）Q 型聚类划分样地，并用轮廓系数和子样本稳定性评价候选 k。
set.seed(123)
ks <- 2:6
q_fits <- lapply(ks, function(k) kmeans(X, centers = k, nstart = 100))
wss <- vapply(q_fits, function(z) z$tot.withinss, numeric(1))
dx <- dist(X)
silhouette_mean <- mapply(function(z, k) mean(silhouette(z$cluster, dx)[, "sil_width"]), q_fits, ks)

set.seed(123)
stability <- vapply(seq_along(ks), function(i) {
  mean(replicate(30, {
    index <- sample(seq_len(nrow(X)), floor(0.8 * nrow(X)))
    subfit <- kmeans(X[index, , drop = FALSE], centers = ks[i], nstart = 50)
    mclust::adjustedRandIndex(q_fits[[i]]$cluster[index], subfit$cluster)
  }))
}, numeric(1))
q_evaluation <- data.frame(k = ks, wss = wss, silhouette = silhouette_mean, stability = stability)
print(q_evaluation)
silhouette_choice <- q_evaluation$k[which.max(q_evaluation$silhouette)]
stability_choice <- q_evaluation$k[which.max(q_evaluation$stability)]
# 两个指标冲突时不机械追求唯一值；本模拟优先采用稳定性最高且肘部图支持的候选。
selected_k <- if (silhouette_choice == stability_choice) silhouette_choice else stability_choice
q_fit <- q_fits[[which(ks == selected_k)]]
cat("轮廓系数候选 k = ", silhouette_choice,
    "；稳定性候选 k = ", stability_choice, "。\n", sep = "")
cat("Q 型聚类选用 k = ", selected_k, "；各簇样本量：\n", sep = "")
print(table(q_fit$cluster))
print(aggregate(features, list(cluster = q_fit$cluster), mean))

# （3）R 型聚类分析变量的相似性/冗余结构。
correlation <- cor(features)
r_distance <- as.dist(1 - abs(correlation))
r_fit <- hclust(r_distance, method = "average")
print(cutree(r_fit, k = 3))

if (!interactive()) grDevices::cairo_pdf(tempfile("exercise-5.4-", fileext = ".pdf"), width = 9, height = 5)
par(mfrow = c(1, 2))
plot(ks, silhouette_mean, type = "b", pch = 19, xlab = "k", ylab = "平均轮廓系数", main = "Q 型聚类候选 k")
plot(r_fit, main = "环境变量的 R 型聚类", xlab = "变量", sub = "距离 = 1 - |r|")
if (!interactive()) dev.off()

# （4）解释边界。
cat("Q 型聚类描述样地环境组合；R 型聚类描述变量共变或冗余。\n")
cat("R 型聚类不能直接给出变量对响应的预测重要性，也不能作为因果或经营决策证据。\n")
