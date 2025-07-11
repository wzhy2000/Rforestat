# 1.
# Covertype数据集下载链接：https://archive.ics.uci.edu/ml/machine-learning-databases/covtype/covtype.data.gz
library(readr)
covertype_data <- read.csv(gzfile("covtype.data.gz"), header = FALSE)
colnames(covertype_data) <- c(
  "Elevation", "Aspect", "Slope", "Horizontal_Distance_To_Hydrology",
  "Vertical_Distance_To_Hydrology", "Horizontal_Distance_To_Roadways",
  "Hillshade_9am", "Hillshade_Noon", "Hillshade_3pm",
  "Horizontal_Distance_To_Fire_Points", paste0("Wilderness_Area_", 1:4),
  paste0("Soil_Type_", 1:40), "Cover_Type"
)
selected_vars <- covertype_data[1:30, 1:6]

# 2.
summary(selected_vars)
scaled_vars <- scale(selected_vars)
summary(scaled_vars)

# 通过比较标准化前后summary函数的输出，可以看到标准化后数据的均值趋近于0，方差趋近于1，不同变量间的量纲差异被消除。

# 3.
pca = princomp(scaled_vars, subset = 1:30)
summary(pca, loadings = TRUE)

# PC1 = 0.513*Elevation + 0.267*Aspect + 0.395*Slope + 0.320*Horizontal_Distance_To_Hydrology + 0.405*Vertical_Distance_To_Hydrology + 0.493*Horizontal_Distance_To_Roadways
# PC2 = 0.248*Elevation - 0.254*Aspect - 0.447*Slope + 0.739*Horizontal_Distance_To_Hydrology - 0.355*Vertical_Distance_To_Hydrology
# PC1主要反映Elevation的特征，PC2主要反映Horizontal_Distance_To_Hydrology的特征

# 4.
screeplot(pca, type = "lines")

# 3个

# 5.
biplot(pca, scale = 0.5)

# PC1得分的差异，本质是样本所在区域“地形复杂度 + 水源垂直可及性 + 人类干扰程度”的综合差异。例如：
# PC1得分高的样本：可能处于“海拔高、坡度大、垂直方向离水源远、远离道路”的区域，这类生境自然干扰强、人类影响弱，更适合耐旱/抗逆性强的植被（如高山针叶林、灌丛）。
# PC2得分的差异，本质是样本所在区域“水源水平可达性 + 坡度-垂直落差的协同效应”的差异。例如：
# PC2得分高的样本：可能处于“水平方向近水源、坡度缓、垂直方向近水源”的区域（如河谷缓坡），这类生境水分极充足、地形平缓，更适合喜湿的森林类型（如落叶阔叶林、河岸林）。
