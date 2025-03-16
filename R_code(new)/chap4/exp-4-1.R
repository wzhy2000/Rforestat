# 载入数据
library(forestat)
data(larch)
names(larch)

# 1 胸径和树高的分布分析
pdf("图4.4a.pdf", width = 8, height = 6, family = "GB1")
par(mar = c(5, 5.5, 4, 2), mgp = c(3.5, 1, 0))
hist(larch$H, breaks = 100, xlab = "树高(m)", ylab = "频数", main =  "", cex.axis = 2.2, cex.lab = 2.2)
dev.off()
pdf("图4.4b.pdf", width = 8, height = 6, family = "GB1")
par(mar = c(5, 5.5, 4, 2), mgp = c(3.5, 1, 0))
hist(larch$D, breaks = 100, xlab = "胸径(cm)", ylab = "频数", main =  "", cex.axis = 2.2, cex.lab = 2.2)
dev.off()
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

# 4 相关性分析
res.corr <- data.frame()
for(i in c("SD", "D", "CLR", "CBH", "CW_E", "CW_W", "CW_S" ,"CW_N")){
  pearson <- cor.test(larch$H, larch[[i]], method = "pearson")
  spearman <- cor.test(larch$H, larch[[i]], method = "spearman", exact = FALSE)
  kendall <- cor.test(larch$H, larch[[i]], method = "kendall")
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
library(dplyr)
res.corr.rounded <- res.corr %>%  mutate_if(is.numeric, round, 4)
print(res.corr.rounded)

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

# 6 单因素方差分析
data <- larch[which(larch$PLOT %in% group1), ]
SD <- factor(data$SD) 
H <- data$H
D <- data$D
model.aov.H <- aov(H ~ SD)
model.aov.D <- aov(D ~ SD)
summary(model.aov.H)
summary(model.aov.D)

# 7 协方差分析
model.cov <- aov(H ~ SD + D)
summary(model.cov)
