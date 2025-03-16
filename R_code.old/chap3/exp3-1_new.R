library(forestat)
data("picea")
library(ggplot2)
library(dplyr)
selected_data <- picea %>% select(Branch, LH, LHCB, LCW)

library(tidyr)
long_data <- selected_data %>%
  pivot_longer(cols = everything(), names_to = "Parameter", values_to = "Value")
long_data$Value <- log10(long_data$Value + 1) 
pdf("图3.11.pdf", width = 10, height = 6, family = "GB1")
p1 <- ggplot(long_data, aes(x = Parameter, y = Value, fill = Parameter)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 21, outlier.size = 3, notch = TRUE) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    x = "Parameter",
    y = "Log10(Value)"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none")
p1
dev.off()

remove_outliers <- function(data, column) {
  Q1 <- quantile(data[[column]], 0.25, na.rm = TRUE) 
  Q3 <- quantile(data[[column]], 0.75, na.rm = TRUE) 
  IQR <- Q3 - Q1                                  
  lower_bound <- Q1 - 1.5 * IQR                   
  upper_bound <- Q3 + 1.5 * IQR                   
  data[
    data[[column]] >= lower_bound & data[[column]] <= upper_bound, 
    ]
}
cleaned_data <- selected_data
for (col in colnames(cleaned_data)) {
  cleaned_data <- remove_outliers(cleaned_data, col)
}

library(PerformanceAnalytics)
pdf("图3.12.pdf", width = 10, height = 6, family = "GB1")
chart.Correlation(cleaned_data[, 1:4])
dev.off()
library(reshape2)
library(viridis)
# correlation_matrix <- cor(cleaned_data)
# correlation_data <- melt(correlation_matrix)
# pdf("图3.12.pdf", width = 10, height = 6, family = "GB1")
# p2 <- ggplot(correlation_data, aes(x = Var1, y = Var2, fill = value)) +
#   geom_tile(color = "#ffffff", lwd = 0.5) + 
#   scale_fill_viridis(
#     option = "H",  
#     direction = 1,  
#     limits = c(-1, 1)
#   ) +geom_text(aes(label = round(value, 2)), color = "black", size = 3, fontface = "bold") +  labs( x = NULL,y = NULL) +
#   theme_minimal(base_size = 10) + theme(
#     axis.text.x = element_text(angle = 45, hjust = 1, size =8, face = "bold"),
#     axis.text.y = element_text(size = 8, face = "bold"),  # Y轴标签
#     panel.grid.major = element_blank(), 
#     panel.grid.minor = element_blank(),
#     legend.position = "NA"  
#   )
# p2
# dev.off()

model <- lm(Branch ~ LH + LHCB, data = cleaned_data)

summary(model)
pdf("图3.13.pdf", width = 16, height = 8, family = "GB1")
par(mfrow = c(1, 2))
par(mar = c(5,5,3,3))
plot(model, which = 1, cex.lab = 2, cex.axis = 2, sub.caption = "", caption = "")
plot(model, which = 2, cex.lab = 2, cex.axis = 2, sub.caption = "", caption = "")
dev.off()