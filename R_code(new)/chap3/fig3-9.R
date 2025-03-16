#mydat <- read.csv("lysdata.csv", fileEncoding = "GBK", sep = ",")
mycols <- c("red", "yellow", "blue", "pink", "green")
p1 <- ggplot(mydat, aes(x = DBH, y = Height)) +
  geom_point(aes(color = Age.Group), size = 1) +  
  labs(x = "胸径(cm)", y = "树高(m)", color = "年龄组别") +
  scale_color_manual(values = mycols) + 
  theme(legend.position = "NA") +
  theme(legend.key = element_rect(fill = "lightblue", color = "black")) +
  theme(legend.position = c(0.9, 0.21))+
  theme(
    axis.title.x = element_text(size = 16), axis.title.y = element_text( size = 16),  
    axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16), 
    legend.title = element_text(size = 16), legend.text = element_text(size = 16)
  )
p1
px <- ggplot(mydat, aes(x = DBH, fill = Age.Group)) +
  geom_density(alpha = 0.6) +
  theme(legend.position = "none") +
  xlab("") +
  ylab("密度") + 
  scale_fill_manual(values = mycols) +
  theme(axis.title.y = element_text( size = 16),
        axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16)
  )
px
py <- ggplot(mydat, aes(y = Height, fill = Age.Group)) +
  geom_density(alpha = 0.6) +
  theme(legend.position = "none") +
  ylab("") + 
  xlab("密度") + 
  scale_fill_manual(values = mycols) +
  theme(axis.title.x = element_text( size = 16),
        axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16)
  )
py

library(gridExtra)
pdf("图3.9.pdf", width = 8, height = 6, family = "GB1")
blankplot <- ggplot() + 
  geom_blank(aes(1, 1)) + 
  theme_void()
grid.arrange(px, blankplot, p1, py, ncol = 2, nrow = 2, 
             widths = c(4, 1.4), heights = c(1.4, 4))
dev.off()
