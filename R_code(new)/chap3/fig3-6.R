#library(forestat)
#data(picea)
pdf("图3.6.pdf", width = 8, height = 6, family = "GB1")
ggplot(picea, aes(x = H0, y = D0)) +
  geom_point(col = "steelblue") +
  geom_smooth(method = "lm", color = "red") +
  scale_y_continuous(limits = c(0, 45)) +           
  scale_x_continuous(breaks = seq(0, 25, 5), limits = c(0, 25), expand = c(0, 0)) +
  labs(x = "树高(m)", 
       y = "胸径(m)") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16),   
        axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16),
        plot.margin = margin(5, 10, 5, 5))
dev.off()