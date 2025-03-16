library(forestat)
data("larch")
names(larch)
################## (1) 使用hist(H)和hist(D)看这两个数据的分布 ##############
pdf("树高分布.pdf", width = 8, height = 5, family = "GB1")
par(mar = c(5,5,3,3))
hist(larch$H, breaks = 100, xlab = "树高", ylab = "频数", main = "", cex.lab = 2, cex.axis = 2)
dev.off()
pdf("胸径分布.pdf", width = 8, height = 5, family = "GB1")
par(mar = c(5,5,3,3))
hist(larch$D, breaks = 100, xlab = "胸径", ylab = "频数", main = "", cex.lab = 2, cex.axis = 2)
dev.off()

#################### （2）对所有样地数据，都判断是否是正态分布 ###############
# 将数据按样地分组
table(larch$PLOT)
# grouped_data <- split(larch$H, larch$PLOT)
# sum(sapply(grouped_data, function(x) length(x) > 10))
plots <- sort(unique(larch$PLOT))

# 样本数大于等于10时，执行Shapiro-Wilk检验，并返回p值
p.values <- lapply(plots, function(x) {
    data <- larch[which(larch$PLOT == x ), c("H")]
    shapiro.test(data)$p.value
})

res.norm <- data.frame(plot = plots, p.value = unlist(p.values))

p.greater05 <- res.norm[which(res.norm$p.value > 0.05), ]
dim(p.greater05)
head(p.greater05[order(p.greater05$p.value, decreasing = TRUE), ])
p.less05 <- res.norm[which(res.norm$p.value < 0.05), ]
dim(p.less05)
head(p.less05[order(p.less05$p.value, decreasing = FALSE), ])

#############  (3)两两样地之间做是否为同一分布的 假设检验 ######################
# 生成样地的两两组合

combinations <- combn(plots, 2)

# 创建一个空数据框
res.ks <- data.frame(
  plot1 = character(),
  plot2 = character(),
  D.statistic = as.numeric(),
  p.value = as.numeric()
)

# 对每一对组合进行 KS 检验
for (i in 1:ncol(combinations)) {
  # 获取样地名称
  plot1 <- combinations[1, i]
  plot2 <- combinations[2, i]
  
  # 提取两块样地的树高数据
  H1 <- larch[which(larch$PLOT == plot1), c("H")]
  H2 <- larch[which(larch$PLOT == plot2), c("H")]

  # 执行 KS 检验
  ks <- ks.test(H1, H2)
  
  # 保存结果
  res.ks <- rbind(res.ks, data.frame(
    plot1 = plot1,
    plot2 = plot2,
    D.statistic = ks$statistic,
    p.value = ks$p.value
  ))
}

# 查看 KS 检验结果
head(res.ks[order(res.ks$p.value, decreasing = TRUE), ]) 
head(res.ks[order(res.ks$p.value, decreasing = FALSE), ])

p.greater05 <- res.ks[which(res.ks$p.value > 0.05), ]
dim(p.greater05)
head(p.greater05[order(p.greater05$p.value, decreasing = TRUE), ])
p.less05 <- res.ks[which(res.ks$p.value < 0.05), ]
dim(p.less05)
head(p.less05[order(p.less05$p.value, decreasing = FALSE), ])


################## (4) 计算所有样地的H与SD, D, CLR, CBH, CW_E, CW_W, CW_S, CW_N的 关联性分析 ##########
# (i)Pearson, (ii) Spearman, (iii) Kendell关联性分析的结果

res.corr <- data.frame(
  name = character(),
  Pearson = numeric(),        # Pearson 相关系数
  Pearson.p = numeric(),      # Pearson 的 p 值
  Spearman = numeric(),       # Spearman 相关系数
  Spearman.p = numeric(),     # Spearman 的 p 值
  Kendall = numeric(),        # Kendall 相关系数
  Kendall.p = numeric()       # Kendall 的 p 值
)

for(i in c("SD", "D", "CLR", "CBH", "CW_E", "CW_W", "CW_S" ,"CW_N")){
  # 执行相关性检验
  pearson <- cor.test(larch$H, larch[[i]], method = "pearson")
  spearman <- cor.test(larch$H, larch[[i]], method = "spearman")
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
res.corr.rounded <- res.corr %>%
  mutate_if(is.numeric, round, 4)
print(res.corr.rounded)

################# (5) 两组样地，对其做树高方差，树高均值是否相等的两个检验 ##############
group1 <-c()
group2 <-c()

for( i in  plots)
{
  if (i %in% c(group1, group2))  next
  if (mean(larch[larch$PLOT == i, "H"]) < 8)
    group1 <- c(group1, i)
  else
    group2 <- c(group2, i)
}

H1 <- larch[which(larch$PLOT %in% group1), c("H")]
H2 <- larch[which(larch$PLOT %in% group2), c("H")]

# 总体方差未知，两正态正态总体的期望值检验，Welch 的 t 检验
t.test(H1, H2)

# 总体期望值未知，两正态正态总体的方差检验，F方差比检验
var.test(H1, H2)


################# (6) 树高H（因变量）与 林分密度SD（自变量）或 胸径D（因变量）与 林分密度SD（自变量）的单因素方差分析 ############
data <- larch[which(larch$PLOT %in% group1), ]  # 在 group1 中的样本数据
SD <- factor(data$SD) 
H <- data$H
D <- data$D

model.aov.H.SD <- aov(H ~ SD)
model.aov.H.D <- aov(H ~ D)

summary(model.aov.H.SD)
summary(model.aov.H.D)

############### (7) 协方差分析 ##############
model.cov <- aov(H ~ SD + D)
summary(model.cov)


 