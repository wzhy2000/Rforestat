library(forestat)
data("picea")

H <- picea$H0
CPA <- picea$CPA

H <- H + runif(length(H), min = -1e-6, max = 1e-6)
CPA <- CPA + runif(length(CPA), min = -1e-6, max = 1e-6)

cor.test(H, CPA, method = "pearson")

cor.test(H, CPA, method = "spearman")

cor.test(H, CPA, method = "kendall")
