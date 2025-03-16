library(boot)
library(forestat)
data("birch")
plot2 <- birch[which(birch$PLOT == 2), ]
tree_heights <- plot2$H

median_function <- function(data, indices) {
  sampled_data <- data[indices] 
  return(median(sampled_data))   
}

set.seed(123)  
boot_result <- boot(tree_heights, statistic = median_function, R = 1000)
boot.ci(boot_result, type = "perc", conf = 0.95)