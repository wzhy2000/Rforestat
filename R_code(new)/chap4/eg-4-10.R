# 问题一
library(forestat)
data(picea)
cor.test(picea$H0, picea$CPA, method = "pearson")

# 问题二
# H <- picea$H0 + runif(length(picea$H0), min = -1e-6, max = 1e-6)
# CPA <- picea$CPA + runif(length(picea$CPA), min = -1e-6, max = 1e-6)
cor.test(picea$H0, picea$CPA, method = "spearman", exact=FALSE)

# 问题三
cor.test(picea$H0, picea$CPA, method = "kendall")
