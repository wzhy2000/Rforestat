data(iris)
x <- iris[iris$Species == "setosa", "Sepal.Length"]
mu <- 4.5

t.test(x, mu = mu)