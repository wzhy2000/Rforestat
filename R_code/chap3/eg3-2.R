#radar_data为eg3-1中的数据
radar_data <- within(radar_data, {H0_type <-NA
    H0_type[H0<=12] <- "smallTrees"
    H0_type[H0>12 & H0<=19] <- "medium-sizedTrees"
    H0_type[H0>19] <- "largeTrees"})