library(forestat)
data("crassifolia")
radar_data <- crassifolia

library(dplyr)
yd_H <- select(radar_data, Plot1, H0)
colnames(yd_H) <- c("Plot", "H")

for(i in 1:nrow(yd_H)){
  if(yd_H[i,1]>=10 & yd_H[i,1]<=16){   
    yd_H[i,1] <- paste("yd", yd_H[i,1], sep = "")
  }else{
    yd_H[i,1] <- paste("yd", 0, yd_H[i,1], sep = "") 
  }
}

remove_outliers <- function(x) {
  Q <- quantile(x, probs=c(.25, .75), na.rm = TRUE)
  iqr <- 1.5 * IQR(x, na.rm = TRUE)
  y <- x
  y[x <= (Q[1] - iqr)] <- NA
  y[x >= (Q[2] + iqr)] <- NA
  y
}
yd_H <- yd_H %>%
  group_by(Plot) %>%
  mutate(H= remove_outliers(H))
yd_H <- na.omit(yd_H)

write.csv(yd_H, file="yd_H.csv", row.names=FALSE)
yd_H <- read.csv("yd_H.csv", header=TRUE, stringsAsFactors=FALSE)

library(ggplot2)
p1 <- ggplot(yd_H, aes(x = Plot, y = H)) +
  geom_boxplot() +
  labs(x = "样地类型",
       y = "实测树高H/m",
       title = "不同样地间实测树高的箱线图") +
  theme_minimal() +
  theme(panel.grid.major = element_blank())
p1

library(ggplot2)
library(ggridges)
p2 <- ggplot(yd_H, aes(x = H, y = Plot, fill = Plot)) +
  geom_density_ridges(alpha = 0.5) + 
  scale_x_continuous(breaks = seq(5,25,5),limits = c(4,25), expand = c(0,0)) +
  labs(x = "实测树高H/m",
       y = "样地类型",
       title = "不同样地间实测树高的山脊线图") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
p2

yd_H1 <- yd_H %>% group_by(Plot) %>% summarise(median=median(H))
p3 <- ggplot(yd_H1, aes(x=median, y=reorder(Plot, median))) +
  geom_point(color="blue", size = 4) +
  geom_segment(aes(x=7, xend=median, y=reorder(Plot, median), yend=reorder(Plot, median)),color="lightgrey") +
  labs (x = "实测树高的中位数/m",
        y = "样地类型",
        title = "不同样地间实测树高中位数的棒棒糖图") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
p3

pdf("BoxPlot.pdf",width = 8,height = 5,family="GB1") 
p1
dev.off()

pdf("Redgeline.pdf",width = 8,height = 5,family="GB1")
p2
dev.off()

pdf("Lollipop.pdf",width = 8,height = 5,family="GB1")
p3
dev.off()