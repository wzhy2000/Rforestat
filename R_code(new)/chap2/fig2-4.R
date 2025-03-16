library(ggplot2)
data(iris)
qplot(data = iris, x = Species, y = Sepal.Length, geom = "boxplot", 
      xlab = "种类", ylab = "萼片长度 (cm)") +
  theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16),   
        axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16)) 