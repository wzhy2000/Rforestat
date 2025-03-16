# 问题一
tab.1st <- data.frame(acid = c(20, 15), neutral = c(16, 4), alkaline = c(30, 15))
rownames(tab.1st) <- c("Disease", "No Disease")
tab.2nd <- data.frame(acid = c(18, 17), neutral = c(26, 4), alkaline = c(20, 15))
rownames(tab.2nd) <- c("Disease", "No Disease")
chisq.test(tab.1st)
chisq.test(tab.2nd)

# 问题二
fisher.test(tab.1st)
fisher.test(tab.2nd)

# 问题三
tab.contingency <- matrix(c(58, 6, 8, 28), nrow = 2)
rownames(tab.contingency) <- c("first_D", "first_0")
colnames(tab.contingency) <- c("second_1", "second_0")
mcnemar.test(tab.contingency, correct = TRUE)