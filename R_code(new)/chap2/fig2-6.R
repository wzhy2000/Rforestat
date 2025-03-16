library(ggplot2)
iris$Species <- as.factor(iris$Species)
pdf("图2.6.pdf", width = 8, height = 6, family = "GB1")
ggplot(iris, aes(x = Species, y = Sepal.Length)) + 
  geom_violin(aes(fill = Species), trim = FALSE, alpha = 0.3) + 
  geom_jitter(aes(color = Species), width = 0.2, size = 3) + 
  stat_smooth(aes(group = 1), method = "lm", se = FALSE, formula = y ~ x, 
              color = "red", linetype = "dashed") + 
  labs(x = "鸢尾花种类", y = "花萼长度(cm)", color = "鸢尾花种类", fill = "鸢尾花种类") + 
  theme_minimal() +
  theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16),   
        axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16),
        legend.title = element_text(size = 16), legend.text = element_text(size = 16))
dev.off()
