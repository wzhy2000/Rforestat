library(forestat)
data(picea)
library(dplyr)
picea1 <- picea %>% group_by(PLOT1) %>% summarise(n())
colnames(picea1) <- c("PLOT", "number")

# par(mfrow = c(1, 2))
pdf("图3.1a.pdf", width = 8, height = 6, family = "GB1")
palette("default")
palette(c(palette(), "turquoise", "purple", "pink", "ivory", "brown", "lightblue", 
          "tan", "orange")) 
barplot(height = picea1$number, names.arg = picea1$PLOT, horiz = TRUE, 
        xlab = "树木样本个数", col = palette(), 
        cex.names = 1.8, cex.lab = 1.8, cex.axis = 1.8)
dev.off()

pdf("图3.1b.pdf", width = 8, height = 6, family = "GB1")
myLabel <- paste("yd", picea1$PLOT, "(", 
                 round(picea1$number / sum(picea1$number)*100, 2), "%)", sep = "")
pie(picea1$number, labels = myLabel, col = palette(),cex = 1.8, radius = 1)
dev.off()