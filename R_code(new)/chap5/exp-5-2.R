library(readr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(factoextra)
library(cluster)
library(dbscan)
library(mclust)


covertype_url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/covtype/covtype.data.gz"
covertype_data <- read_csv(covertype_url, col_names = FALSE, show_col_types = FALSE)
colnames(covertype_data) <- c(
      "Elevation", "Aspect", "Slope", 
      "Horizontal_Distance_To_Hydrology", "Vertical_Distance_To_Hydrology",
      "Horizontal_Distance_To_Roadways", "Hillshade_9am", "Hillshade_Noon", 
      "Hillshade_3pm", "Horizontal_Distance_To_Fire_Points",
      paste0("Wilderness_Area_", 1:4), paste0("Soil_Type_", 1:40), "Cover_Type"
)
cat("原始数据维度:", dim(covertype_data), "\n")
cat("植被类型分布:\n")
table(covertype_data$Cover_Type)


set.seed(123)
sample_size <- 5000
sample_indices <- sample(nrow(covertype_data), sample_size)
covertype_sample <- covertype_data[sample_indices, ]
env_features <- covertype_sample[, 1:10]
true_cover_types <- covertype_sample$Cover_Type
cat("抽样后数据维度:", dim(env_features), "\n")
table(true_cover_types)

summary(env_features)
env_scaled <- scale(env_features)


dist_matrix <- dist(env_scaled, method = "euclidean")
cat("距离矩阵维度:", attr(dist_matrix, "Size"), "\n")
dist_values <- as.vector(dist_matrix)
summary(dist_values)



wss_values <- sapply(1:10, function(k) {
      set.seed(123)
      kmeans(env_scaled, centers = k, nstart = 25)$tot.withinss
    })
print(data.frame(k = 1:10, WCSS = round(wss_values, 0)))


sil_values <- sapply(2:10, function(k) {
      set.seed(123)
      km_result <- kmeans(env_scaled, centers = k, nstart = 25)
      sil <- silhouette(km_result$cluster, dist_matrix)
      mean(sil[, 3])
    })
print(data.frame(k = 2:10, Silhouette = round(sil_values, 3)))


library(factoextra)
p1_auto <- fviz_nbclust(env_scaled, kmeans, method = "wss", k.max = 10)
p2_auto <- fviz_nbclust(env_scaled, kmeans, method = "silhouette", k.max = 10)
print(p1_auto) 
print(p2_auto) 


set.seed(123)
optimal_k <- 7
kmeans_result <- kmeans(env_scaled, centers = optimal_k, nstart = 25)
cat("K-means聚类结果:\n")
print(table(kmeans_result$cluster))


hc_result <- hclust(dist_matrix, method = "ward.D2")
hc_clusters <- cutree(hc_result, k = optimal_k)
cat("层次聚类结果:\n")
print(table(hc_clusters))


ari_kmeans <- adjustedRandIndex(kmeans_result$cluster, true_cover_types)
ari_hc <- adjustedRandIndex(hc_clusters, true_cover_types)
cat("聚类算法与真实标签的ARI比较:\n")
cat("K-means ARI:", round(ari_kmeans, 3), "\n")
cat("层次聚类 ARI:", round(ari_hc, 3), "\n")



sil_kmeans <- silhouette(kmeans_result$cluster, dist_matrix)
sil_hc <- silhouette(hc_clusters, dist_matrix)
cat("内部评估 - 平均轮廓系数:\n")
cat("K-means:", round(mean(sil_kmeans[, 3]), 3), "\n")
cat("层次聚类:", round(mean(sil_hc[, 3]), 3), "\n")


p1 <- fviz_cluster(kmeans_result, data = env_scaled,
                   ellipse.type = "confidence", geom = "point",
                   palette = "Set2", main = "K-means Clustering Results",
                   ggtheme = theme_minimal())
print(p1)


set.seed(123)
subsample_size <- 200
subsample_indices <- sample(nrow(env_scaled), subsample_size)
env_subsample <- env_scaled[subsample_indices, ]
hc_subsample <- hclust(dist(env_subsample), method = "ward.D2")


p2 <- fviz_dend(hc_subsample, k = optimal_k, cex = 0.8,
                k_colors = "Set2", color_labels_by_k = TRUE,
                main = "Hierarchical Clustering Dendrogram (Representative bsample)")
print(p2)

comparison_data <- data.frame(
  PC1 = prcomp(env_scaled)$x[, 1],
  PC2 = prcomp(env_scaled)$x[, 2],
  K_means = as.factor(kmeans_result$cluster),
  Hierarchical = as.factor(hc_clusters)
)
library(gridExtra)
p3 <- ggplot(comparison_data, aes(PC1, PC2, color = K_means)) +
  geom_point(alpha = 0.7) + labs(title = "K-means") + theme_minimal()
p4 <- ggplot(comparison_data, aes(PC1, PC2, color = Hierarchical)) +
  geom_point(alpha = 0.7) + labs(title = "Hierarchical") + theme_minimal()
grid.arrange(p3, p4, ncol = 2)


cluster_env_summary <- aggregate(env_features, 
                                 by = list(Cluster = kmeans_result$cluster), 
                                 FUN = mean)
