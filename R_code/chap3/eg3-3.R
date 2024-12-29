#radar_data为eg3-1中的数据
for(i in 1:nrow(radar_data)){
  rownames(radar_data)[i] <- paste("Tree", i, seq=" ")
}