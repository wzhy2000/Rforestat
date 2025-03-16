set.seed(123)
m <- matrix(sample(1:10, 6, replace = TRUE), nrow = 2)
rownames(m) <- c("第一行", "第二行")
colnames(m) <- c("第一列", "第二列", "第三列")
m
rbind(cbind(m, "行汇总" = rowSums(m)), "列汇总" = c(colSums(m), sum(colSums(m))))
