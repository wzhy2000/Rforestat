p <- ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point(aes(color = Species)) +
  labs(x = "萼片长度 (cm)", y = "萼片宽度 (cm)", color = "Species") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16),   
        axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16),
        legend.title = element_text(size = 16), legend.text = element_text(size = 16))
print(p)