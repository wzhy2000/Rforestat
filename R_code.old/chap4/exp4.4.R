x = iris[iris$Species=="setosa",1]
y = iris[iris$Species=="versicolor",1]
var.test(x, y)