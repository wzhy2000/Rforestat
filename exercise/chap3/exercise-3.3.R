library(dplyr)
library(forestat)
data("picea")
picea1 <- picea %>% group_by(PLOT1) %>% summarise(Count = n())
colnames(picea1) <- c("PLOT", "number")
barplot(height = picea1$number, names.arg = picea1$PLOT, horiz = TRUE,
        xlab = "树木样本个数", col = rainbow(length(picea1$PLOT)))
labels <- paste("yd", picea1$PLOT, "(", round(picea1$number / sum(picea1$number) * 100, 2), "%)", sep = "")
pie(picea1$number, labels = labels, col = rainbow(length(picea1$PLOT)))
