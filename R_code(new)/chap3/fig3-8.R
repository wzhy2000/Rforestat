#mydat <- read.csv("lysdata.csv", fileEncoding = "GBK", sep = ",")
library(ggplot2)
library(ggpointdensity)
mydat$Age.Group <- factor(mydat$Age.Group, 
                          levels = c("幼龄林", "中龄林", "近熟林","成熟林","过熟林"))
pdf("图3.8.pdf", width = 8, height = 6, family = "GB1")
ggplot(mydat, aes(x = DBH, y = Height)) +
  geom_pointdensity(size = 2) +
  scale_color_viridis_c(option = "turbo") +
  geom_smooth(method = "loess", formula = y ~ x, color = "blue", fill = "red", 
              linetype = "dashed", size = 1.7) +
  facet_grid(~ Age.Group, scales = "free") +
  labs(
    x = "胸径(cm)",
    y = "树高(m)",
    color = "数据点密度"
  ) +
  theme(
    axis.title.x = element_text(size = 16), axis.title.y = element_text( size = 16),  
    axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16), 
    legend.title = element_text(size = 16), legend.text = element_text(size = 16),
    strip.text = element_text(size = 16)
  ) + 
  scale_x_continuous(breaks = seq(0, max(mydat$DBH, na.rm = TRUE), by = 4)) + 
  scale_y_continuous(breaks = seq(0, max(mydat$Height, na.rm = TRUE), by = 5))
dev.off()
