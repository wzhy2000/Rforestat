#library(forestat)
#data(picea)
library(dplyr)
picea1 <- select(picea, LH, OBS, PLOT1)
colnames(picea1) <- c("H", "Obs", "PLOT")
picea2 <- select(picea, H0, OBS, PLOT1)
colnames(picea2) <- c("H", "Obs", "PLOT")
type <- rep(c("雷达观测树高(m)", "实测树高(m)"), each = 402)
radar_bind <- cbind(rbind(picea1, picea2), type)
head(radar_bind, n = 3)



library(ggplot2)
pdf("图3.4a.pdf", width = 8, height = 6, family = "GB1")
p1 <- ggplot(radar_bind, aes(x = type, y = H, color = type)) +
  geom_boxplot(outlier.size = 0.7) +
  geom_line(aes(group = Obs), color = "grey80", size = 0.05) +
  geom_point(size = 0.7) +
  labs(x = "",
       y = "树高(m)") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),   
        axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20),
        legend.position = "none")
p1
dev.off()
pdf("图3.4b.pdf", width = 8, height = 6, family = "GB1")
p2 <- ggplot(radar_bind, aes(x = type, y = H, color = type)) +
  geom_violin(size = 1) +
  geom_line(aes(group = Obs), color = "grey80", size = 0.05) +
  geom_point(size = 0.7, color = 'black') +
  labs(x = "",
       y = "树高(m)") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),   
        axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20),
        legend.position = "none")
p2
dev.off()

library(cowplot)
plot_grid(p1, p2)