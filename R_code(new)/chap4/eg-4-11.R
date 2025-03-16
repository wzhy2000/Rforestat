tree <- data.frame(
  X = c(2, 4, 3, 2, 4, 7, 7, 2, 2, 5, 4, 
        5, 6, 8, 5, 10, 7, 12, 12, 6, 6, 
        7, 11, 6, 6, 7, 9, 5, 5, 10, 6, 3, 10),
  A = factor(rep(1:3, c(11, 10, 12))))
tree.aov <- aov(X ~ A, data = tree)
summary(tree.aov)