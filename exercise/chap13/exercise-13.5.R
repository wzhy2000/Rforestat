# 习题 13.5：UCI Covertype 的朴素贝叶斯分类

library(e1071)
library(caret)

locate_covertype <- function() {
  candidates <- c(Sys.getenv("COVTYPE_FILE", unset = ""), "covtype.data.gz", "covertype.zip")
  candidates <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (length(candidates) > 0L) return(candidates[[1]])
  options(timeout = max(600, getOption("timeout")))
  destination <- file.path(tempdir(), "covertype.zip")
  message("未找到本地 Covertype 文件，正在从 UCI 官方下载约 11 MB 数据……")
  download.file("https://archive.ics.uci.edu/static/public/31/covertype.zip", destination, mode = "wb")
  destination
}
read_covertype <- function(path) {
  if (grepl("\\.zip$", path, ignore.case = TRUE)) {
    listing <- unzip(path, list = TRUE)
    member <- listing$Name[grepl("covtype\\.data(\\.gz)?$", listing$Name, ignore.case = TRUE)][1]
    if (is.na(member)) stop("UCI ZIP 中未找到 covtype.data 或 covtype.data.gz。")
    extracted <- unzip(path, files = member, exdir = tempdir(), overwrite = TRUE)
    if (grepl("\\.gz$", extracted, ignore.case = TRUE)) read.csv(gzfile(extracted), header = FALSE) else read.csv(extracted, header = FALSE)
  } else if (grepl("\\.gz$", path, ignore.case = TRUE)) {
    read.csv(gzfile(path), header = FALSE)
  } else {
    read.csv(path, header = FALSE)
  }
}

column_names <- c(
  "Elevation", "Aspect", "Slope", "Horizontal_Distance_To_Hydrology",
  "Vertical_Distance_To_Hydrology", "Horizontal_Distance_To_Roadways",
  "Hillshade_9am", "Hillshade_Noon", "Hillshade_3pm",
  "Horizontal_Distance_To_Fire_Points", paste0("Wilderness_Area_", 1:4),
  paste0("Soil_Type_", 1:40), "Cover_Type"
)
covtype <- read_covertype(locate_covertype())
stopifnot(nrow(covtype) == 581012L, ncol(covtype) == length(column_names))
names(covtype) <- column_names
covtype$Cover_Type <- factor(covtype$Cover_Type, levels = 1:7)
xvars <- c(
  "Elevation", "Slope", "Horizontal_Distance_To_Hydrology",
  "Vertical_Distance_To_Hydrology", "Horizontal_Distance_To_Roadways",
  "Horizontal_Distance_To_Fire_Points"
)
print(table(covtype$Cover_Type))

set.seed(123)
training_index <- createDataPartition(covtype$Cover_Type, p = 0.8, list = FALSE)
training <- covtype[training_index, c(xvars, "Cover_Type")]
testing <- covtype[-training_index, c(xvars, "Cover_Type")]
preprocessor <- preProcess(training[xvars], method = c("medianImpute", "center", "scale"))
x_training <- predict(preprocessor, training[xvars])
x_testing <- predict(preprocessor, testing[xvars])

model <- naiveBayes(x_training, training$Cover_Type)
prediction <- factor(predict(model, x_testing), levels = levels(testing$Cover_Type))
confusion <- confusionMatrix(prediction, testing$Cover_Type)
print(confusion$table)
cat("测试集总体准确率：", unname(confusion$overall["Accuracy"]), "\n")
cat("宏平均召回率：", mean(confusion$byClass[, "Sensitivity"], na.rm = TRUE), "\n")

off_diagonal <- as.data.frame(confusion$table)
off_diagonal <- off_diagonal[off_diagonal$Prediction != off_diagonal$Reference, ]
off_diagonal <- off_diagonal[order(-off_diagonal$Freq), ]
cat("频数最高的五类错误：\n"); print(head(off_diagonal, 5), row.names = FALSE)
within_class_correlation <- lapply(split(training, training$Cover_Type), function(z) cor(z[xvars]))
max_cor <- vapply(within_class_correlation, function(m) max(abs(m[upper.tri(m)])), numeric(1))
print(data.frame(Cover_Type = names(max_cor), max_absolute_predictor_correlation = max_cor), row.names = FALSE)
cat("较强的类内预测变量相关会违背条件独立假设，影响朴素贝叶斯概率校准。\n")
