library(ggplot2)
library(RColorBrewer)
library(ggpubr)
mydata <- read.csv("lysdata.csv", fileEncoding = "GBK", sep = ",")
attach(mydata)
ggplot(mydata, aes(x = Age.Group, y = Height, fill = Age.Group)) +
  geom_jitter(width = 0.3, size = 2, alpha = 0.6, shape = 21, stroke = 0.5) +
  geom_boxplot(outlier.shape = NA, color = "black", alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 18, color = "red", size = 3) +
  stat_compare_means(comparisons = list(c("幼龄林", "中龄林"), c("中龄林", "过熟林"), c("幼龄林", "近熟林"), c("成熟林", "过熟林")), label = "p.signif", method = "t.test", size = 5, vjust = 0.5, color = "red") +scale_fill_brewer(palette = "Pastel1") +
  labs(x = "Age Group", y = "Tree Height (m)", fill = "Age group") +
  theme_minimal(base_size = 14) +
  theme(
    text = element_text(family = "serif"),             
    axis.title.x = element_text(face = "bold", size = 14), 
    axis.title.y = element_text(face = "bold", size = 14),  
    axis.text.x = element_text(angle = 45, hjust = 1, size =8), 
    axis.text.y = element_text(size = 12), 
    legend.position = "top", 
    panel.grid.major = element_line(color = "grey80", size = 0.5), 
    panel.grid.minor = element_blank()
  ) +scale_y_continuous(breaks = seq(0, max(mydata$Height, na.rm = TRUE), by = 5))

library(ggpointdensity)
mydata$Age.Group <- factor(mydata$Age.Group, levels = c("幼龄林", "中龄林", "近熟林","成熟林","过熟林"))
ggplot(mydata, aes(x = DBH, y = Height)) +
  geom_pointdensity(size = 2) +
  scale_color_viridis_c(option = "turbo") +
  geom_smooth(method = "loess", formula = y ~ x, color = "blue", fill = "red", linetype = "dashed", size = 1.7) +
  facet_grid(~ Age.Group, scales = "free") +
  labs(
    x = "Diameter at Breast Height (cm)",
    y = "Tree Height (m)"
  ) +scale_x_continuous(breaks = seq(0, max(mydata$DBH, na.rm = TRUE), by = 4)) +scale_y_continuous(breaks = seq(0, max(mydata$Height, na.rm = TRUE), by = 5))


mycols <- c("red", "yellow", "blue", "pink", "green")
p1 <- ggplot(mydata, aes(x = DBH, y = Height)) +
  geom_point(aes(color = Age.Group), size = 1) +  
  labs( x = "DBH(cm)", y = "Height(m)") +
  scale_color_manual(values = mycols)+theme(legend.position = "NA")+
  theme(legend.key = element_rect(fill = "lightblue", color = "black")) +
  theme(legend.position = c(0.9, 0.21))
p1
px <- ggplot(mydata, aes(x = DBH, fill = Age.Group)) +
  geom_density(alpha = 0.6) +
  theme(legend.position = "none") +
  xlab("")+scale_fill_manual(values = mycols)
px
py <- ggplot(mydata, aes(y = Height, fill = Age.Group)) +
  geom_density(alpha = 0.6) +
  theme(legend.position = "none") +
  ylab("")+scale_fill_manual(values = mycols)
py
library( "gridExtra")
blankPlot<-ggplot()+geom_blank(aes(1,1))+ theme_void()
grid.arrange(px, blankPlot, p1, py,ncol=2, nrow=2, widths=c(4, 1.4), heights=c(1.4, 4))

