# 1 R软件包安装和数据载入
#install.packages("devtools")
devtools::install_github("caf-ifrit/forestat/forestat")
help(package = "forestat")
vignette("forestat")
library(forestat)
data()
data("forestData")
data("plot_1")


# 2 数据结构与统计分析
str(forestData)
median(forestData$AGE)
mean(forestData$S)
max(forestData$BA)
min(forestData$Bio)
summary(forestData)
library(dplyr)
selected_data <- select(forestData, ID, AGE, H, S, BA, Bio)
head(selected_data)


# 3 图形绘制与保存
plot(forestData$Bio, forestData$H, main = "Biomass vs Height", 
     xlab = "Biomass", ylab = "Height")

png("scatter_plot.png", width = 650, height = 450)
hist(forestData$AGE, main = "Age Distribution", xlab = "Age", xlim = c(0, 120))
dev.off()

#install.packages("ggplot2")
library(ggplot2)
plot_1_filtered <- plot_1[plot_1$age_group != 0, ]
x <- as.factor(plot_1_filtered$age_group)
ggplot(plot_1_filtered, aes(x, y = standing_stock)) + 
  geom_boxplot() + 
  labs(title = "Standing Stock by Age Group", x = "Age Group", y = "Standing Stock") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

violin_plot <- ggplot(plot_1_filtered, aes(x, y = average_diameter_at_breast_height)) + 
  geom_violin(trim = FALSE, fill = "skyblue") + 
  geom_jitter(width = 0.2, color = "blue", alpha = 0.5) + 
  labs(title = "Average Diameter at Breast Height Distribution by Age Group", 
       x = "Age Group", y = "Average Diameter at Breast Height (cm)") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
violin_plot

write.csv(plot_1_filtered, file = "plot_1_filtered.csv", row.names = FALSE)
ggsave("violin_plot.pdf", violin_plot, width = 11, height = 8, units = "in")

#install.packages("export")
library(export)
graph2pdf(file = "violin_plot.pdf")
graph2ppt(file = "violin_plot.pptx")
graph2svg(file = "violin_plot.svg")
table2excel(plot_1_filtered, file = "plot_1_filtered.xlsx", digits = 4, 
            digitspvals = 1, sheetName = "Anova_table", add.rownames = TRUE)
table2html(plot_1_filtered, file = "plot_1_filtered.html")