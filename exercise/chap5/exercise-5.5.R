# 1. 加载库与数据
library(psych)
library(GPArotation)
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"
data <- read.csv(url, header = FALSE)
colnames(data)[2:32] <- c("diagnosis", paste0(rep(c("radius","texture","perimeter","area","smoothness",
                                                    "compactness","concavity","concave_points","symmetry","fractal_dim"), 
                                                  each=3), rep(c("_mean","_se","_worst"))))

# 2. 选取6个特征并标准化
df <- data[, c("radius_mean", "texture_mean", "perimeter_mean", "area_mean", 
               "smoothness_mean", "compactness_mean")]
df_scaled <- scale(df)  # 标准化

# 3. 检验因子分析适用性
KMO(df_scaled)  # KMO检验

# 4. 确定因子数
fa.parallel(df_scaled, fa = "both", n.iter = 100)  # 碎石图与平行分析

# 5. 主成分法（PCA）提取因子（旋转前后）
pca_unrotated <- principal(df_scaled, nfactors = 2, rotate = "none")
pca_rotated <- principal(df_scaled, nfactors = 2, rotate = "varimax")
print(loadings(pca_unrotated))  # PCA旋转前
print(loadings(pca_rotated))    # PCA旋转后

# 6. 最大似然法（MLE）提取因子（旋转前后）
mle_unrotated <- factanal(df_scaled, factors = 2, rotation = "none")
mle_rotated <- factanal(df_scaled, factors = 2, rotation = "varimax")
print(loadings(mle_unrotated))  # MLE旋转前
print(loadings(mle_rotated))    # MLE旋转后

