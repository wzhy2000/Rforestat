library("forestat")
data(larch)
attach(larch)

x <- larch[larch$plot == "28", "D"]
y <- larch[larch$plot == "71", "D"]

set.seed(123)
x <- sample(x, 20)
y <- sample(y, 20)

t.test(x, y, var.equal = TRUE) # 假设方差相等
