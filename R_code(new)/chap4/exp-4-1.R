# 载入数据
library(forestat)
data(larch)
names(larch)

# 1 胸径和树高的分布分析
# pdf("图4.4a.pdf", width = 8, height = 6, family = "GB1")
par(mar = c(5, 5.5, 4, 2), mgp = c(3.5, 1, 0))
hist(larch$H, breaks = 100, xlab = "树高(m)", ylab = "频数", main =  "", cex.axis = 2.2, cex.lab = 2.2)
# dev.off()
# pdf("图4.4b.pdf", width = 8, height = 6, family = "GB1")
par(mar = c(5, 5.5, 4, 2), mgp = c(3.5, 1, 0))
hist(larch$D, breaks = 100, xlab = "胸径(cm)", ylab = "频数", main =  "", cex.axis = 2.2, cex.lab = 2.2)
# dev.off()
# 2 正态性检验
plots <- sort(unique(larch$PLOT))
p.values <- lapply(plots, function(x) {
  data <- larch[which(larch$PLOT == x ), c("H")]
  shapiro.test(data)$p.value
})
res.norm <- data.frame(plot = plots, p.value = unlist(p.values))
p.greater05 <- res.norm[which(res.norm$p.value > 0.05), ]
dim(p.greater05)
head(p.greater05[order(p.greater05$p.value, decreasing = TRUE), ], 3)

# 3 两两分布检验
combinations <- combn(plots, 2)
res.ks <- data.frame()
for (i in 1:ncol(combinations)) {
  plot1 <- combinations[1, i]
  plot2 <- combinations[2, i]
  H1 <- larch[which(larch$PLOT == plot1), c("H")]
  H2 <- larch[which(larch$PLOT == plot2), c("H")]
  ks <- ks.test(H1, H2)
  res.ks <- rbind(res.ks, data.frame(
    plot1 = plot1,
    plot2 = plot2,
    D.statistic = ks$statistic,
    p.value = ks$p.value
  ))}
p.greater05 <- res.ks[which(res.ks$p.value > 0.05), ]
dim(p.greater05)
head(p.greater05[order(p.greater05$p.value, decreasing = TRUE), ], 3)

# 3.1 两两 K--S 检验的 BH 多重校正
res.ks$p.value.BH <- p.adjust(res.ks$p.value, method = "BH")
p.greater05.BH <- res.ks[which(res.ks$p.value.BH > 0.05), ]
dim(p.greater05.BH)
head(p.greater05.BH[order(p.greater05.BH$p.value.BH, decreasing = TRUE), ], 3)

# 4 相关性分析：以样地为独立单位
# 同一样地的树木可能相关，故先在样地内对各变量取均值，
# 再对样地均值进行相关性检验。
corr.vars <- c("H", "SD", "D", "CLR", "CBH", "CW_E", "CW_W", "CW_S", "CW_N")
larch.plot <- aggregate(larch[, corr.vars], by = list(PLOT = larch$PLOT),
                        FUN = mean, na.rm = TRUE)
n.plot <- nrow(larch.plot)
print(n.plot)

res.corr <- data.frame()
for(i in setdiff(corr.vars, "H")){
  pearson <- cor.test(larch.plot$H, larch.plot[[i]], method = "pearson")
  spearman <- cor.test(larch.plot$H, larch.plot[[i]], method = "spearman", exact = FALSE)
  kendall <- cor.test(larch.plot$H, larch.plot[[i]], method = "kendall", exact = FALSE)
  res.corr <- rbind(res.corr, data.frame(
    name = paste("H", i, sep = "."),
    Pearson = pearson$estimate,
    Pearson.p = pearson$p.value,
    Spearman = spearman$estimate,
    Spearman.p = spearman$p.value,
    Kendall = kendall$estimate,
    Kendall.p = kendall$p.value
  ))
}
res.corr.rounded <- res.corr
estimate.cols <- c("Pearson", "Spearman", "Kendall")
p.value.cols <- c("Pearson.p", "Spearman.p", "Kendall.p")
res.corr.rounded[estimate.cols] <- lapply(res.corr[estimate.cols], round, digits = 4)
res.corr.rounded[p.value.cols] <- lapply(
  res.corr[p.value.cols],
  function(x) ifelse(x < 0.0001, "< 0.0001", formatC(x, format = "f", digits = 4))
)
print(res.corr.rounded, row.names = FALSE)

# 5 两样本均值与方差检验
group1 <- c(); group2 <- c()
for( i in  plots)
{
  if (mean(larch[larch$PLOT == i, "H"]) < 8)
    group1 <- c(group1, i)
  else
    group2 <- c(group2, i)
}

H.group1 <- larch[which(larch$PLOT %in% group1), c("H")]
H.group2 <- larch[which(larch$PLOT %in% group2), c("H")]
t.test(H.group1, H.group2)
var.test(H.group1, H.group2)

# 6 单因素方差分析：以样本1中的样地为实验单位
# 林分密度为连续的样地层变量。对样本1的树木数据先按样地取均值，
# 再检验样地平均树高和平均胸径与林分密度的线性关联。
sample1.data <- larch[larch$PLOT %in% group1, ]
plot.data1 <- aggregate(cbind(H, D, SD) ~ PLOT, data = sample1.data,
                        FUN = mean, na.rm = TRUE)

model.aov.H <- aov(H ~ SD, data = plot.data1)
model.aov.D <- aov(D ~ SD, data = plot.data1)

print(nrow(plot.data1))

summary(model.aov.H)
summary(model.aov.D)



# 7 协方差分析：继续使用样本1的样地均值数据，并在必要时保留交互项
# 对连续自变量中心化，使主效应对应另一变量处于样本均值时的关系。
plot.data1$D_c <- as.numeric(scale(plot.data1$D, center = TRUE, scale = FALSE))
plot.data1$SD_c <- as.numeric(scale(plot.data1$SD, center = TRUE, scale = FALSE))

# 未控制林分密度时，检验胸径与树高的总体线性关联。
model.total <- lm(H ~ D_c, data = plot.data1)
anova(model.total)

# 比较加性模型与含交互项模型，检验胸径—树高关系是否随林分密度变化。
model.add <- lm(H ~ D_c + SD_c, data = plot.data1)
model.int <- lm(H ~ D_c * SD_c, data = plot.data1)
anova(model.add, model.int)

# 交互项不显著时，正式拟合并报告共同斜率的协方差分析模型。
model.ancova <- lm(H ~ D_c + SD_c, data = plot.data1)
anova(model.ancova)
# summary(model.ancova)
