#library(forestat)
#data(picea)
for(i in 1:nrow(picea)){
   rownames(picea)[i] <- paste("Tree", i, sep = " ")
  }
head(rownames(picea))
