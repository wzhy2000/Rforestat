library(tidyr)
data <- read.csv("data-{eg4-10}.csv")
head(data)
# 将数据转换为长格式
data_long_tree <- data %>%
  pivot_longer(
    cols = c(Height0, Height.after.3.years, Height.after.6.years),  # 需要转换的列
    names_to = "time",       # 新列名称，用来存储原始列名（表示时间）
    values_to = "Height"     # 新列名称，用来存储高度值
  )

data_long_tree <- data_long_tree[, c("plot", "sampleid", "time", "Height")]
data_long_tree$plot <- as.factor(data_long_tree$plot)
data_long_tree$sampleid <- as.factor(data_long_tree$sampleid)
data_long_tree$time <- as.factor(data_long_tree$time)

fit <- aov(Height ~ plot * time + Error(sampleid/(plot * time)), data = data_long_tree)

summary(fit)





# N
N <- 16

# t
t <- nlevels(data_long_tree$time)

# a 
a <- length(unique(data_long_tree$plot))

# bar{y}
mean <- mean(data_long_tree$Height)

# 计算 y_i.. (plot的均值)
plot_means <- aggregate(Height ~ plot, data = data_long_tree, FUN = base::mean)

# 计算 y_..k (time的均值)
time_means <- aggregate(Height ~ time, data = data_long_tree, FUN = base::mean)

# 计算 y_ij. (每个plot中每个样本点的均值)
plot_sample_means <- aggregate(Height ~ plot + sampleid, data = data_long_tree, FUN = base::mean)

# 计算y_i.k
plot_time_means <- aggregate(Height ~ plot + time, data = data_long_tree, FUN = base::mean)

# SS_T (总平方和)
SS_T <- sum(data_long_tree$Height^2) - N * t * mean^2
SS_T

# SS_A (plot的平方和)
SS_A <- t * sum(4 * plot_means$Height^2) - N * t * mean^2
SS_A

# SS_E(A)
SS_EA <- t * sum(plot_sample_means$Height^2) - t * sum(4 * plot_means$Height^2)
SS_EA

# SS_B
SS_B <- N * sum(time_means$Height^2) - N * t * mean^2
SS_B

# SS_AB
SS_AB <- sum(4 * plot_time_means$Height^2) - N * t * mean^2 - SS_A - SS_B
SS_AB

# SS_E(B) (B的误差平方和)  psu网址中没有写
# SS_EB <- sum(data_long$Height^2) - sum(plot_time_means$Height^2)
SS_EB <- sum(data_long_tree$Height^2)  - sum(4 * plot_time_means$Height^2) - SS_EA
SS_EB

SS_A + SS_B + SS_EA + SS_EB + SS_AB
