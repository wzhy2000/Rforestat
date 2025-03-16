# 1 数据载入
library(forestat)
data(picea)
library(dplyr)
selected.data <- picea %>% select(BRANCH, LH, LHCB, LCW)

# 2 数据可视化
library(tidyr)
long_data <- selected.data %>%
  pivot_longer(cols = everything(), names_to = "Parameter", values_to = "Value")
long_data$Value <- log10(long_data$Value + 1) 
p1 <- ggplot(long_data, aes(x = Parameter, y = Value, fill = Parameter)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 21, outlier.size = 3, 
               notch = TRUE) +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "参数",
       y = "Log10(Value)"
  ) +
  theme_minimal(base_size = 15) +
  theme(axis.title.x = element_text(size = 16), 
        axis.title.y = element_text( size = 16),  
        axis.text.x = element_text(size = 16), 
        axis.text.y = element_text(size = 16),
        legend.position = "none")
p1

# 3 异常值剔除
removeOutliers <- function(data, column) {
  Q1 <- quantile(data[[column]], 0.25, na.rm = TRUE) 
  Q3 <- quantile(data[[column]], 0.75, na.rm = TRUE) 
  IQR <- Q3 - Q1                                  
  lower_bound <- Q1 - 1.5 * IQR                   
  upper_bound <- Q3 + 1.5 * IQR                   
  data[data[[column]] >= lower_bound & data[[column]] <= upper_bound, ]
}
cleaned_data <- selected.data
for (col in colnames(cleaned_data)) {
  cleaned_data <- removeOutliers(cleaned_data, col)
}

# 4 变量相关性分析
library(PerformanceAnalytics)
chart.Correlation(cleaned_data[, 1:4])

# 5 模型构建
model <- lm(BRANCH ~ LH + LHCB, data = cleaned_data)
summary(model)
# par(mfrow = c(1, 2))
pdf("图3.13a.pdf", width = 8, height = 6, family = "GB1")
par(mar = c(4.5, 5.5, 4, 2), mgp = c(3.5, 1, 0))
plot(model, which = 1, cex.axis = 2.2, cex.lab = 2.2, ) 
dev.off()
pdf("图3.13b.pdf", width = 8, height = 6, family = "GB1")
par(mar = c(4.5, 5.5, 4, 2), mgp = c(3.5, 1, 0))
plot(model, which = 2, cex.axis = 2.2, cex.lab = 2.2)
dev.off()

# 6 保存图形
pdf("变量箱线图.pdf", width = 10, height = 6, family = "GB1")
p1
dev.off()
pdf("变量相关性分析.pdf", width = 10, height = 6, family = "GB1")
library(PerformanceAnalytics)
chart.Correlation(cleaned_data[, 1:4])
dev.off()