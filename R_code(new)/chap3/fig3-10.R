#library(forestat)
#data(picea)
estimateA <- function(x) {
  A = -9.21 * log(2.5 * (1/x) - 0.03)
  A
}
picea <- transform(picea, A = estimateA(D0)) 

library(dplyr)
picea <- picea %>% group_by(A) %>% mutate(A, H = mean(H0)) %>% arrange(A)
picea1 <- select(picea, A, H)  
pdf("图3.10.pdf", width = 12, height = 6, family = "GB1")
par(mar = c(5, 5.5, 3, 2), mgp = c(3.5, 1, 0))
plot(H ~ A, picea1, type = "l", 
     xlab = "树木生长年龄(年)", ylab = "树高(m)", xlim = c(15, 30), cex.axis = 1.5, cex.lab = 1.5)
dev.off()
