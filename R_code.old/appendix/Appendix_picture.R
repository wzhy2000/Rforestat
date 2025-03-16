library(ggplot2)
library(RColorBrewer)
library(ggpubr)
mydata <- read.csv("lysdata.csv", fileEncoding = "GBK", sep = ",")
attach(mydata)
# 图1
pdf("图1.pdf",width = 8,height = 6, family = "GB1")
ggplot(mydata, aes(x = Height, fill = Age.Group)) +
  geom_density(alpha = 0.4) +  
  labs(title = "", x = "DBH(cm)", y = "Height(m)") 
dev.off()

# 图2
pdf("图2.pdf",width = 8,height = 6, family = "GB1")
ggplot(mydata, aes(x = DBH, y = Height, color = Height)) +
  geom_point(size = 3) +
  scale_color_gradient(low = "lightblue", high = "darkblue")+
  labs( x = "DBH(cm)", y = "Height(m)") 
dev.off()

# 图3
pdf("图3.pdf",width = 8,height = 6, family = "GB1")
ggplot(mydata, aes(x = DBH, y = Height)) +
  geom_point(aes(color = Age.Group), size =1.5, shape =8 ) +  
  labs(x="DBH(cm)",y= "Height(m)")+geom_smooth(method="loess",se=TRUE,color="#af2934")
dev.off()

# 图4
pdf("图4.pdf",width = 8,height = 6, family = "GB1")
ggplot(mydata, aes(x = Age.Group, y = Height, fill = Age.Group)) +
  geom_violin(trim = TRUE) +labs( x ="Age Group", y = "Height(m)")+theme_bw()+
  theme(legend.position ="bottom")
dev.off()

# 图5
pdf("图5.pdf",width = 8,height = 6, family = "GB1")
library(ggbeeswarm)
ggplot(mydata, aes(x = Age.Group, y = Height, color = Age.Group))  +
  geom_beeswarm(size=0.8,cex = 0.5, priority = "descending") +
  scale_color_manual(values = c("#AF0F11", "#3372A6", "#367B34", "#7F4288", 
                                "#D16800", "#D9D92D", "#976C53")) +
  scale_y_continuous(position = "left")  + 
  theme(axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        axis.ticks = element_line(),
        axis.text = element_text(color = "black")) +labs( x = "Age Group", y = "Height(m)") 
dev.off()

# 图6
pdf("图6.pdf",width = 8,height = 6, family = "GB1")
ggplot(mydata, aes(x = DBH, y = Height)) +
  geom_point(aes(color = Age.Group), size = 3) +
  labs(title = "",x = "DBH(cm)", y = "Height(m)") +theme_linedraw()
dev.off()

# 图7
pdf("图7.pdf",width = 8,height = 6, family = "GB1")
library(ggrepel)
ggplot(mydata, aes(x = DBH, y = Height, label = Height)) +
  geom_point( color ="#66C2A5") +
  geom_text_repel(family = "serif", color = "black", size = 3, box.padding = 0.3, point.padding = 0.2)+
  labs(x = "DBH(cm)", y = "Height(m)",family = "serif") 
dev.off()

# 图8
pdf("图8.pdf",width = 8,height = 6, family = "GB1")
library(viridis)
ggplot(mydata, aes(x = DBH, y =  Height, color =  Height)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red", se = FALSE) + 
  scale_color_viridis(option = "viridis")  + 
  labs(x = "DBH (cm)", y = "Height (m)") +
  annotate("text", x = 4, y = 25, label = expression(italic(R)^2~"= 0.66"), hjust = 0, size = 5) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "right",
    panel.grid.minor = element_blank()
  ) +  theme(plot.title = element_text(face = "bold"))
dev.off()

#图9
pdf("图9.pdf",width = 8,height = 6, family = "GB1")
library(viridisLite)
ggplot(mydata, aes(x = DBH, y = Height, color = Height)) +
  geom_point(size = 3) +
  scale_color_viridis(option = "plasma") +labs(x = "DBH(cm)", y = "Height(m)")+theme_dark()
dev.off()

# 图10
pdf("图10.pdf",width = 8,height = 6, family = "GB1")
ggplot(mydata, aes(x = Age.Group, y = Height, fill = Age.Group)) +
  geom_violin(trim = TRUE, alpha = 0.7, color = "black", size = 0.5) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, 
               outlier.size = 2, width = 0.2, color = "black", 
               fill = "white", alpha = 0.5, size = 0.5) +
  labs( x = "Age Group",
        y = "Height (m)",
        fill = "Age Group",
        color = "Density") +
  scale_x_discrete(breaks = unique(mydata$age.group), 
                   labels = unique(mydata$age.group)) +
  scale_y_continuous(breaks = seq(0, max(mydata$Height, na.rm = TRUE), by = 5)) +
  theme_minimal(base_size = 16) + 
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 12),legend.text = element_text(size = 10),    axis.title.x = element_text(face = "bold", size = 14),axis.title.y = element_text(face = "bold", size = 14),axis.text = element_text(size = 12),plot.title = element_text(face = "bold", size = 20, hjust = 0.5),plot.subtitle = element_text(size = 14, hjust = 0.5, color = "grey50"),    panel.grid.minor = element_blank(),panel.grid.major = element_line(color = "grey80", size = 0.5)) +theme(panel.background = element_rect(fill = "white", color = "black", size = 1))
dev.off()

# 图11
pdf("图11.pdf",width = 8,height = 6, family = "GB1")
mydata_cor <- mydata[,c("DBH","Height","CBH","CW_E","CW_S","CW_W","CW_N","SD","CLR","CW","CW_EW","CW_SN")]
cor_matrix <- cor(mydata_cor, use = "pairwise.complete.obs")
library(reshape2)
cor_data <- melt(cor_matrix)
names(cor_data) <- c("Variable1", "Variable2", "Correlation")
ggplot(cor_data, aes(x = Variable1, y = Variable2, fill = Correlation, size = abs(Correlation))) + 
  geom_point(shape = 21) +
  scale_fill_distiller(palette = "RdYlBu", limits = c(-1, 1)) + 
  labs(x = "Variables", y = "Variables", fill = "Correlation", size = "Correlation\nStrength") +theme_linedraw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

# 图12
pdf("图12.pdf",width = 8,height = 6, family = "GB1")
library(ggpointdensity)
ggplot(mydata, aes(x = DBH, y = Height)) +
  geom_pointdensity(size = 3) +
  scale_color_viridis_c(option = "turbo") +
  labs( x = "DBH(cm)", y = "Height(m)")
dev.off()

# 图13
pdf("图13.pdf",width = 8,height = 6, family = "GB1")
mydata$Age.Group <- factor(mydata$Age.Group, levels = c("幼龄林", "中龄林", "近熟林","成熟林","过熟林"))
ggplot(mydata, aes(x = DBH, y = Height, color = Age.Group)) +
  geom_point() +
  facet_grid(Age.Group ~ .) +
  labs(title = "Scatter Plot of DBH vs Height by Age Group", x = "DBH", y = "Height")
dev.off()

#图14
pdf("图14.pdf",width = 8,height = 6, family = "GB1")
ggplot(data = mydata, aes(x = DBH, y = Height, color = factor(Age.Group))) +
  geom_point() +
  ggtitle("Tree DBH vs Height") +
  labs(subtitle = "Forest Survey Data", caption = "Data source: mydata") +
  theme(
    text = element_text(family = "serif"),
    plot.title = element_text(face = "bold", size = 16, color = "darkred"),
    plot.subtitle = element_text(face = "italic", size = 12, color = "darkblue"),
    plot.caption = element_text(size = 10, color = "gray50"),
    axis.title.x = element_text(size = 14, color = "purple"),
    axis.title.y = element_text(size = 14, color = "purple"),
    axis.text.x = element_text(size = 10, color = "gray40"),
    axis.text.y = element_text(size = 10, color = "gray40"),
    legend.title = element_text(face = "bold", size = 12, color = "blue"),
    legend.text = element_text(size = 10, color = "darkgreen"))
dev.off()

# 图15
pdf("图15.pdf",width = 8,height = 6, family = "GB1")
library(ggpointdensity)
ggplot(data = mydata, aes(x = DBH, y = Height)) +
  geom_pointdensity(size = 3) +
  scale_color_viridis_c(option = "inferno") +
  theme(
    axis.text.x = element_text(family = "serif", size = 10, color = "gray40"),
    axis.text.y = element_text(family = "serif", size = 10, color = "gray40")
  )+
  labs(x = "DBH(cm)", y = "Height(m)")
dev.off()

#图16
pdf("图16.pdf",width = 8,height = 6, family = "GB1")
library(ggpointdensity)
ggplot(mydata, aes(x = DBH, y = Height, color = Height)) +
  geom_point(size = 3) +
  scale_color_viridis(option = "plasma") +
  labs(x = "DBH(cm)", y = "Height(m)")
dev.off()

# 图17
pdf("图17.pdf",width = 8,height = 6, family = "GB1")
library(tidyverse)
library(reshape2)
library(psych)
library(viridis)
lysdata <- read.csv("lysdata.csv", fileEncoding = "GB18030")
lysdata_continuous <- lysdata %>%
  select(-c(1, 2, 18)) %>%     
  select(where(is.numeric))     
pp <- corr.test(lysdata_continuous[, 1:2], lysdata_continuous %>% select(-c(1:2)), 
                method = "pearson", adjust = "fdr")
cor <- pp$r
pvalue <- pp$p
myfun <- function(pval) {
  stars <- ""
  if (pval <= 0.001) stars <- "***"
  else if (pval <= 0.01) stars <- "**"
  else if (pval <= 0.05) stars <- "*"
  stars}
heatmap_data <- melt(cor) %>%
  rename(tree = Var1, other = Var2, cor = value) %>%
  mutate(pvalue = melt(pvalue)[, 3]) %>%
  mutate(signif = sapply(pvalue, myfun))
ggplot(heatmap_data, aes(tree, other, color = cor)) +
  geom_tile(color = "grey70", fill = "white", size = 1) +
  geom_point(aes(size = abs(cor)), shape = 15) +
  geom_text(aes(label = signif), size = 6, color = "white", hjust = 0.5, vjust = 0.7) +
  labs(x = NULL, y = NULL, color = NULL) +
  scale_color_viridis_c() +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme(
    text = element_text(family = ""),  
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    panel.border = element_rect(fill = NA, color = "grey70", size = 2, linetype = "solid")
  ) +
  scale_size(range = c(1, 10), guide = NULL) +
  guides(color = guide_colorbar(direction = "vertical", reverse = FALSE,
                                barwidth = unit(0.5, "cm"), barheight = unit(10, "cm")))
dev.off()

# 图18
pdf("图18.pdf",width = 8,height = 6, family = "GB1")
ggplot(mydata, aes(x = Age.Group, y = Height, color = Age.Group)) +
  geom_jitter(width = 0.5, height = 0.2, size = 1.2) +
  labs( x = "DBH(cm)", y = "Height(m)")  +
  scale_color_manual(values = c("#AF0F11", "#3372A6", "#367B34", "#7F4288", 
                                "#D16800", "#D9D92D", "#976C53"))+theme_minimal()+theme(legend.position ="NA")
dev.off()
    