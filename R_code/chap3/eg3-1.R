setwd("/path/to/data")
library(xlsx)
#雷达反演数据.xlsx即为data3-1.xlsx
radar_data <- read.xlsx("雷达反演数据.xlsx", 1, header=TRUE)