library(ggplot2)
library(RColorBrewer)
library(ggpubr)
mydat <- read.csv("lysdata.csv", fileEncoding = "GBK", sep = ",")
attach(mydat)
pdf("图3.7.pdf", width = 8, height = 6, family = "GB1")
ggplot(mydat, aes(x = Age.Group, y = Height, fill = Age.Group)) +
  geom_jitter(width = 0.3, size = 2, alpha = 0.6, shape = 21, stroke = 0.5) +
  geom_boxplot(outlier.shape = NA, color = "black", alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 18, color = "red", size = 3) +
  stat_compare_means(comparisons = list(c("幼龄林", "中龄林"), c("中龄林", "过熟林"), 
                                        c("幼龄林", "近熟林"), c("成熟林", "过熟林")), label = "p.signif", 
                     method = "t.test", size = 5, vjust = 0.5, color = "red") +
  scale_fill_brewer(palette = "Pastel1") +
  labs(x = "龄组", 
       y = "树高(m)", 
       fill = "龄组") +
  theme_minimal(base_size = 14) +
  theme(             
    axis.title.x = element_text(size = 16), axis.title.y = element_text( size = 16),  
    axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16), 
    legend.title = element_text(size = 16), legend.text = element_text(size = 16),
    legend.position = "top", 
    panel.grid.major = element_line(color = "grey80", size = 0.5), 
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(breaks = seq(0, max(mydat$Height, na.rm = TRUE), by = 5))
dev.off()
