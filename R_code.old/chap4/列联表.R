first_year <- data.frame(acid = c(20, 15), neutral = c(16, 4), alkaline = c(30, 15))
rownames(first_year) <- c("Disease", "No Disease")
second_year <- data.frame(acid = c(18, 17), neutral = c(26, 4), alkaline = c(20, 15))
rownames(second_year) <- c("Disease", "No Disease")

# Pearson chi^2 独立性检验
chisq.test(first_year)
chisq.test(second_year)

# Fisher精确检验
fisher.test(first_year)
fisher.test(second_year)

# McNemar 检验
data <- matrix(c(58, 6, 8, 28), nrow = 2)
rownames(data) <- c("first_1", "first_0")
colnames(data) <- c("second_1", "second_0")
mcnemar.test(data, correct = TRUE)
