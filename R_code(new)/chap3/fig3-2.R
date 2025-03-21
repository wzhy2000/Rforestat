#library(forestat)
#data(picea)
pdf("图3.2.pdf", width = 12, height = 6, family = "GB1")
par(mar = c(5, 5.5, 4, 2), mgp = c(3.5, 1, 0))
boxplot(CW ~ PLOT1, picea, xlab = "样地类型", ylab = "实测冠幅(m)", cex.labs = 2, cex.lab = 2)
dev.off()
