# ========================================================================
# 林业统计建模与R语言
# 第5章 多元统计分析 习题参考答案
# ========================================================================

# 加载所需的R包
library(ggplot2)
library(dplyr)
library(factoextra)
library(cluster)
library(corrplot)
library(gridExtra)
library(RColorBrewer)
library(readr)
library(mclust)

# ========================================================================
# 习题5.2：使用R语言的内置数据集mtcars进行聚类分析预处理练习
# ========================================================================

# 1. 加载mtcars数据集，选择前6个连续型变量
data(mtcars)
cat("原始mtcars数据集维度:", dim(mtcars), "\n")
cat("变量名称:", names(mtcars), "\n\n")

# 选择前6个连续型变量
mtcars_features <- mtcars[, 1:6]  # mpg, cyl, disp, hp, drat, wt
cat("选择的特征变量:\n")
print(names(mtcars_features))
print(summary(mtcars_features))

# 2. 对数据进行Z-score标准化处理，比较标准化前后数据的分布特征
mtcars_scaled <- scale(mtcars_features)

cat("\n标准化后数据描述性统计:\n")
print(summary(mtcars_scaled))

# 绘制标准化前后数据分布比较图
create_distribution_comparison <- function(original, scaled, title_prefix) {
  # 转换为长格式数据
  original_long <- reshape2::melt(as.data.frame(original), variable.name = "Variable", value.name = "Value")
  original_long$Type <- "Original"
  
  scaled_long <- reshape2::melt(as.data.frame(scaled), variable.name = "Variable", value.name = "Value")
  scaled_long$Type <- "Standardized"
  
  combined_data <- rbind(original_long, scaled_long)
  
  ggplot(combined_data, aes(x = Value, fill = Type)) +
    geom_histogram(alpha = 0.7, position = "identity", bins = 15) +
    facet_wrap(~Variable, scales = "free") +
    scale_fill_manual(values = c("Original" = "skyblue", "Standardized" = "orange")) +
    labs(title = paste(title_prefix, "数据分布比较"),
         x = "数值", y = "频数", fill = "数据类型") +
    theme_minimal() +
    theme(strip.text = element_text(size = 10))
}

# 绘制分布比较图
if(require(reshape2)) {
  p1 <- create_distribution_comparison(mtcars_features, mtcars_scaled, "标准化前后")
  print(p1)
} else {
  cat("需要安装reshape2包来绘制分布比较图\n")
}

# 3. 计算两种距离矩阵
cat("\n计算两种距离矩阵...\n")

# 欧氏距离
dist_euclidean <- dist(mtcars_scaled, method = "euclidean")
cat("欧氏距离矩阵维度:", attr(dist_euclidean, "Size"), "\n")

# 曼哈顿距离
dist_manhattan <- dist(mtcars_scaled, method = "manhattan")
cat("曼哈顿距离矩阵维度:", attr(dist_manhattan, "Size"), "\n")

# 4. 绘制距离分布直方图
cat("\n绘制距离分布比较图...\n")

# 提取距离值
euclidean_values <- as.vector(dist_euclidean)
manhattan_values <- as.vector(dist_manhattan)

# 创建距离分布数据框
dist_data <- data.frame(
  Distance = c(euclidean_values, manhattan_values),
  Type = rep(c("Euclidean", "Manhattan"), 
            each = length(euclidean_values))
)

# 绘制距离分布直方图
p2 <- ggplot(dist_data, aes(x = Distance, fill = Type)) +
  geom_histogram(alpha = 0.7, bins = 20, position = "identity") +
  facet_wrap(~Type, scales = "free") +
  scale_fill_brewer(type = "qual", palette = "Set2") +
  labs(title = "两种距离度量的分布比较",
       x = "距离值", y = "频数") +
  theme_minimal()

print(p2)

# 距离统计摘要
cat("\n三种距离度量的统计摘要:\n")
cat("欧氏距离:\n")
print(summary(euclidean_values))
cat("\n曼哈顿距离:\n")
print(summary(manhattan_values))

# 距离相关性分析
dist_correlation <- cor(data.frame(
  Euclidean = euclidean_values,
  Manhattan = manhattan_values
))

cat("\n两种距离度量的相关系数矩阵:\n")
print(round(dist_correlation, 2))

# 距离度量选择指导
cat("1. 欧氏距离：\n")
cat("   - 适用场景：变量量纲一致、独立性假设成立、数据呈球状分布\n")
cat("   - 优点：直观易懂，计算简单\n")
cat("   - 缺点：对量纲敏感，假设变量独立\n\n")

cat("2. 曼哈顿距离：\n")
cat("   - 适用场景：高维稀疏数据、存在噪声和异常值\n")
cat("   - 优点：对异常值敏感性低，适合高维数据\n")
cat("   - 缺点：不考虑变量间相关性\n\n")

# ========================================================================
# 习题5.3：使用UCI机器学习库的Wine数据集比较多种最优簇数确定方法
# ========================================================================

# 1. 加载Wine数据集
# 直接从UCI下载
wine_url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data"

tryCatch({
  wine_data <- read.csv(wine_url, header = FALSE)
  cat("成功从UCI下载Wine数据集\n")
})

# 如果是从UCI下载的真实数据，设置列名
if(ncol(wine_data) == 14) {
  colnames(wine_data) <- c("class", "alcohol", "malic_acid", "ash", "alcalinity", 
                          "magnesium", "phenols", "flavanoids", "nonflavanoids",
                          "proanthocyanins", "color_intensity", "hue", "od280", "proline")
}

cat("Wine数据集维度:", dim(wine_data), "\n")
cat("真实类别分布:\n")
print(table(wine_data[,1]))

# 提取化学成分特征变量（去除类别标签）
wine_features <- wine_data[, -1]  # 去除第一列的类别标签
wine_true_labels <- wine_data[, 1]

# 标准化特征
wine_scaled <- scale(wine_features)

cat("\nWine特征变量标准化完成\n")
cat("特征变量数量:", ncol(wine_features), "\n")

# 2. 两种最优簇数确定方法

# 肘部法则
calculate_wss <- function(data, k_range) {
  wss_values <- sapply(k_range, function(k) {
    set.seed(123)
    if(k == 1) {
      # k=1时，WSS等于总的方差
      sum(scale(data, scale = FALSE)^2)
    } else {
      kmeans(data, centers = k, nstart = 25)$tot.withinss
    }
  })
  return(wss_values)
}

# 轮廓系数法
calculate_silhouette <- function(data, k_range) {
  sil_values <- sapply(k_range[k_range > 1], function(k) {
    set.seed(123)
    km_result <- kmeans(data, centers = k, nstart = 25)
    sil <- silhouette(km_result$cluster, dist(data))
    mean(sil[, 3])
  })
  return(sil_values)
}


# 3. 测试k=2到k=10的所有可能簇数
k_range <- 1:10
k_range_sil <- 2:10

cat("\n计算肘部法则...\n")
wss_values <- calculate_wss(wine_scaled, k_range)

cat("计算轮廓系数法...\n")
sil_values <- calculate_silhouette(wine_scaled, k_range_sil)


# 绘制两种方法的结果比较图

# 肘部法则图
p3 <- ggplot(data.frame(k = k_range, WSS = wss_values), aes(x = k, y = WSS)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(title = "肘部法则 (Elbow Method)", x = "簇数 k", y = "簇内平方和 (WSS)") +
  theme_minimal() +
  scale_x_continuous(breaks = k_range)

# 轮廓系数法图
p4 <- ggplot(data.frame(k = k_range_sil, Silhouette = sil_values), 
             aes(x = k, y = Silhouette)) +
  geom_line(color = "green", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(title = "轮廓系数法 (Silhouette Method)", x = "簇数 k", y = "平均轮廓系数") +
  theme_minimal() +
  scale_x_continuous(breaks = k_range_sil)

# 组合图形
grid.arrange(p3, p4, ncol = 1)

# 4. 确定最优簇数并分析

# 肘部法则：寻找拐点
wss_diff <- diff(wss_values)
wss_diff2 <- diff(wss_diff)
elbow_k <- which.max(wss_diff2) + 1
cat("\n肘部法则建议的最优簇数:", elbow_k, "\n")

# 轮廓系数法：选择最大值
optimal_k_sil <- k_range_sil[which.max(sil_values)]
cat("轮廓系数法建议的最优簇数:", optimal_k_sil, "\n")

cat("真实的葡萄酒类别数: 3\n")

# 两种方法的结果汇总
results_summary <- data.frame(
  Method = c("肘部法则", "轮廓系数法", "真实类别"),
  Optimal_k = c(elbow_k, optimal_k_sil, 3)
)

cat("\n最优簇数确定结果汇总:\n")
print(results_summary)

# 5. 使用最优簇数进行聚类验证
optimal_k_final <- optimal_k_sil  # 选择轮廓系数法的结果
set.seed(123)
wine_kmeans <- kmeans(wine_scaled, centers = optimal_k_final, nstart = 25)

# 计算ARI评估聚类效果
wine_ari <- adjustedRandIndex(wine_kmeans$cluster, wine_true_labels)
cat("\n使用", optimal_k_final, "个簇进行K-means聚类的ARI:", round(wine_ari, 3), "\n")

# ========================================================================
# 习题5.4：森林环境数据聚类分析应用研究
# ========================================================================

# 1. 模拟森林样地环境数据
set.seed(123)
n_sites <- 200  # 200个森林样地

calculate_wss <- function(data, k_range) {
  wss_values <- sapply(k_range, function(k) {
    set.seed(123)
    if(k == 1) {
      # k=1时，WSS等于总的方差
      sum(scale(data, scale = FALSE)^2)
    } else {
      kmeans(data, centers = k, nstart = 25)$tot.withinss
    }
  })
  return(wss_values)
}

# 创建三种不同的森林环境类型
# 类型1：低海拔阔叶林（50个样地）
# 类型2：中海拔针阔混交林（80个样地）  
# 类型3：高海拔针叶林（70个样地）

create_forest_data <- function() {
  # 类型1：低海拔阔叶林
  type1 <- data.frame(
    elevation = rnorm(50, 800, 100),      # 海拔：700-900m
    slope = rnorm(50, 15, 5),             # 坡度：10-20°
    aspect = rnorm(50, 180, 60),          # 坡向：南坡为主
    soil_ph = rnorm(50, 6.5, 0.5),       # 土壤pH：微酸性
    annual_temp = rnorm(50, 15, 2),       # 年均温：13-17°C
    annual_precip = rnorm(50, 1200, 200), # 年降水量：1000-1400mm
    forest_type = 1
  )
  
  # 类型2：中海拔针阔混交林
  type2 <- data.frame(
    elevation = rnorm(80, 1500, 200),     # 海拔：1300-1700m
    slope = rnorm(80, 25, 8),             # 坡度：15-35°
    aspect = rnorm(80, 135, 90),          # 坡向：多样化
    soil_ph = rnorm(80, 5.8, 0.6),       # 土壤pH：酸性
    annual_temp = rnorm(80, 10, 2.5),     # 年均温：7-13°C
    annual_precip = rnorm(80, 1000, 250), # 年降水量：750-1250mm
    forest_type = 2
  )
  
  # 类型3：高海拔针叶林
  type3 <- data.frame(
    elevation = rnorm(70, 2200, 300),     # 海拔：1900-2500m
    slope = rnorm(70, 30, 10),            # 坡度：20-40°
    aspect = rnorm(70, 90, 120),          # 坡向：多样化
    soil_ph = rnorm(70, 5.2, 0.4),       # 土壤pH：强酸性
    annual_temp = rnorm(70, 5, 3),        # 年均温：2-8°C
    annual_precip = rnorm(70, 800, 300),  # 年降水量：500-1100mm
    forest_type = 3
  )
  
  # 合并数据
  forest_data <- rbind(type1, type2, type3)
  
  # 添加一些随机性和边界约束
  forest_data$slope <- pmax(0, pmin(50, forest_data$slope))
  forest_data$aspect <- ((forest_data$aspect %% 360) + 360) %% 360
  forest_data$soil_ph <- pmax(4, pmin(8, forest_data$soil_ph))
  forest_data$annual_temp <- pmax(-5, pmin(25, forest_data$annual_temp))
  forest_data$annual_precip <- pmax(200, pmin(2000, forest_data$annual_precip))
  
  return(forest_data)
}

forest_data <- create_forest_data()
cat("模拟森林环境数据创建完成\n")
cat("数据维度:", dim(forest_data), "\n")
cat("真实森林类型分布:\n")
print(table(forest_data$forest_type))

# 2. 设计完整的聚类分析流程

# 数据清理
cat("\n=== 数据清理 ===\n")
# 检查缺失值
cat("缺失值检查:\n")
print(colSums(is.na(forest_data)))

# 检查异常值
cat("\n环境变量描述性统计:\n")
print(summary(forest_data[, -7]))  # 排除forest_type列

# 特征选择
cat("\n=== 特征选择 ===\n")
env_vars <- forest_data[, 1:6]  # 选择环境变量
true_types <- forest_data$forest_type

# 变量相关性分析
cor_matrix <- cor(env_vars)
cat("环境变量相关性矩阵:\n")
print(round(cor_matrix, 3))

# 绘制相关性热图
if(require(corrplot)) {
  corrplot(cor_matrix, method = "color", type = "upper", 
           tl.cex = 0.8, tl.col = "black",
           title = "森林环境变量相关性矩阵")
}

# 标准化
cat("\n=== 数据标准化 ===\n")
env_scaled <- scale(env_vars)
cat("标准化完成，所有变量均值为0，标准差为1\n")

# 最优簇数确定
cat("\n=== 最优簇数确定 ===\n")
k_test_range <- 2:10

# 使用多种方法确定最优簇数
wss_forest <- calculate_wss(env_scaled, k_test_range)
sil_forest <- calculate_silhouette(env_scaled, k_test_range)

# 绘制最优簇数确定图
p5 <- ggplot(data.frame(k = k_test_range, WSS = wss_forest), aes(x = k, y = WSS)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(title = "森林环境数据 - 肘部法则", x = "簇数 k", y = "WSS") +
  theme_minimal()

p6 <- ggplot(data.frame(k = k_test_range, Sil = sil_forest), aes(x = k, y = Sil)) +
  geom_line(color = "green", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(title = "森林环境数据 - 轮廓系数法", x = "簇数 k", y = "轮廓系数") +
  theme_minimal()

grid.arrange(p5, p6, ncol = 2)

optimal_k_forest <- 3
# 肘部法则显示WSS随簇数增加而递减，在k=3处出现明显拐点，表明此时增加簇数带来的聚类改善效果开始显著减弱。
# 轮廓系数法则显示k=2时达到最高值（0.283），k=3时略有下降但仍保持较高水平（0.22），随后逐渐降低。
# 综合考虑统计指标和生态学合理性，我们选择k=3作为最优簇数。
cat("最优簇数为:", optimal_k_forest, "\n")

# 聚类实施
cat("\n=== 聚类实施 ===\n")
# K-means聚类
set.seed(123)
kmeans_forest <- kmeans(env_scaled, centers = optimal_k_forest, nstart = 25)

# 层次聚类
dist_forest <- dist(env_scaled, method = "euclidean")
hc_forest <- hclust(dist_forest, method = "ward.D2")
hc_clusters_forest <- cutree(hc_forest, k = optimal_k_forest)

cat("K-means聚类结果:\n")
print(table(kmeans_forest$cluster))
cat("\n层次聚类结果:\n")
print(table(hc_clusters_forest))

# 结果评估
cat("\n=== 聚类结果评估 ===\n")

# 内部评估
sil_kmeans_forest <- silhouette(kmeans_forest$cluster, dist_forest)
sil_hc_forest <- silhouette(hc_clusters_forest, dist_forest)

cat("内部评估 - 平均轮廓系数:\n")
cat("K-means:", round(mean(sil_kmeans_forest[, 3]), 3), "\n")
cat("层次聚类:", round(mean(sil_hc_forest[, 3]), 3), "\n")

# 外部评估
ari_kmeans_forest <- adjustedRandIndex(kmeans_forest$cluster, true_types)
ari_hc_forest <- adjustedRandIndex(hc_clusters_forest, true_types)

cat("\n外部评估 - 调整兰德指数:\n")
cat("K-means ARI:", round(ari_kmeans_forest, 3), "\n")
cat("层次聚类 ARI:", round(ari_hc_forest, 3), "\n")

# 可视化
cat("\n=== 聚类结果可视化 ===\n")

# 使用前两个主成分进行可视化
pca_result <- prcomp(env_scaled)
pca_data <- data.frame(
  PC1 = pca_result$x[, 1],
  PC2 = pca_result$x[, 2],
  True_Type = as.factor(true_types),
  K_means = as.factor(kmeans_forest$cluster),
  Hierarchical = as.factor(hc_clusters_forest)
)

# 真实类型分布
p7 <- ggplot(pca_data, aes(PC1, PC2, color = True_Type)) +
  geom_point(size = 2, alpha = 0.7) +
  labs(title = "真实森林类型分布", color = "森林类型") +
  theme_minimal()

# K-means聚类结果
p8 <- ggplot(pca_data, aes(PC1, PC2, color = K_means)) +
  geom_point(size = 2, alpha = 0.7) +
  labs(title = "K-means聚类结果", color = "聚类簇") +
  theme_minimal()

# 层次聚类结果
p9 <- ggplot(pca_data, aes(PC1, PC2, color = Hierarchical)) +
  geom_point(size = 2, alpha = 0.7) +
  labs(title = "层次聚类结果", color = "聚类簇") +
  theme_minimal()

grid.arrange(p7, p8, p9, ncol = 1)

# 3. R聚类和Q聚类分析

cat("\n=== R聚类（样地聚类）分析 ===\n")
# R聚类：对样地进行聚类
r_cluster_result <- kmeans_forest$cluster

# 分析各簇的环境特征
r_cluster_summary <- aggregate(env_vars, by = list(Cluster = r_cluster_result), FUN = mean)
cat("R聚类 - 各簇环境特征均值:\n")
print(round(r_cluster_summary[, -1], 2))

cat("\n=== Q聚类（环境因子聚类）分析 ===\n")
# Q聚类：对环境变量进行聚类
env_dist <- dist(t(env_scaled))  # 转置后计算距离，对变量进行聚类
q_cluster <- hclust(env_dist, method = "ward.D2")

# 绘制环境因子聚类树状图
plot(q_cluster, main = "环境因子聚类树状图（Q聚类）", 
     xlab = "环境因子", ylab = "距离")

# 将环境因子分为3组
q_cluster_groups <- cutree(q_cluster, k = 3)
cat("Q聚类结果 - 环境因子分组:\n")
for(i in 1:3) {
  factors_in_group <- names(q_cluster_groups)[q_cluster_groups == i]
  cat("组", i, ":", paste(factors_in_group, collapse = ", "), "\n")
}

# 生态学解释
cat("\n=== Q聚类的生态学意义分析 ===\n")

# 分析各组变量的生态学联系
group1_vars <- names(q_cluster_groups)[q_cluster_groups == 1]
group2_vars <- names(q_cluster_groups)[q_cluster_groups == 2]
group3_vars <- names(q_cluster_groups)[q_cluster_groups == 3]

cat("第1组变量生态学分析:\n")
if("elevation" %in% group1_vars && "slope" %in% group1_vars) {
  cat("- 地形因子组：海拔和坡度密切相关\n")
  cat("- 生态意义：共同决定山地森林的立地条件\n")
  cat("- 应用价值：地形因子是森林类型划分的基础\n")
}

cat("\n第2组变量生态学分析:\n")
if("aspect" %in% group2_vars) {
  cat("- 坡向因子：独立性强，与其他因子相关性低\n")
  cat("- 生态意义：主要影响光照和风向，作用相对独立\n")
  cat("- 应用价值：在局部尺度上影响森林微环境\n")
}

cat("\n第3组变量生态学分析:\n")
if(all(c("soil_ph", "annual_temp", "annual_precip") %in% group3_vars)) {
  cat("- 气候-土壤因子组：温度、降水和土壤pH密切相关\n")
  cat("- 生态意义：气候条件直接影响土壤发育和化学性质\n")
  cat("- 应用价值：决定森林植被类型和生长条件\n")
}

# 计算组间和组内相关性
cat("\n=== 环境因子相关性分析 ===\n")
for(i in 1:3) {
  group_vars <- names(q_cluster_groups)[q_cluster_groups == i]
  if(length(group_vars) > 1) {
    group_cor <- cor(env_vars[, group_vars])
    avg_cor <- mean(group_cor[upper.tri(group_cor)])
    cat("第", i, "组内平均相关系数:", round(avg_cor, 3), "\n")
  }
}

# 变量重要性评估
cat("\n=== 变量重要性评估 ===\n")
var_importance <- sapply(names(env_vars), function(var) {
  # 计算该变量与其他变量的平均相关系数
  other_vars <- setdiff(names(env_vars), var)
  mean(abs(cor(env_vars[, var], env_vars[, other_vars])))
})

var_importance_sorted <- sort(var_importance, decreasing = TRUE)
cat("变量重要性排序（基于与其他变量的平均相关性）:\n")
for(i in 1:length(var_importance_sorted)) {
  cat(i, ".", names(var_importance_sorted)[i], ":", round(var_importance_sorted[i], 3), "\n")
}

# 4. 森林经营管理建议

# 略

