data(larch)
# 获取所有唯一的SD值
unique_sd <- sort(unique(larch$SD))

# 创建一个数据框来存储结果
results <- data.frame(
  sd_lower = numeric(),
  sd_upper = numeric(),
  n = numeric(),
  p_value_H = numeric(),
  p_value_D = numeric()
)

# 遍历所有可能的区间
for(i in unique_sd) {
  for(j in unique_sd[unique_sd >= i]) {
    # 获取当前区间的数据
    subset <- larch[larch$SD >= i & larch$SD <= j, ]
    
    # 如果样本量太小，跳过
    if(nrow(subset) < 3) next
    
    # Shapiro-Wilk检验
    p_H <- shapiro.test(subset$H)$p.value
    p_D <- shapiro.test(subset$D)$p.value
    
    # 存储结果
    results <- rbind(results, 
                     data.frame(sd_lower = i,
                                sd_upper = j,
                                n = nrow(subset),
                                p_value_H = p_H,
                                p_value_D = p_D))
  }
}
valid_combinations <- subset(results, p_value_H > 0.05)



# 获取所有唯一的SD值
unique_sd <- sort(unique(larch$SD))

# 创建结果列表
results <- list()
result_count <- 1

# 生成并测试所有可能的组合
for(i in 1:length(unique_sd)) {
  # 获取所有i个数的组合
  combs <- combn(unique_sd, i)
  
  # 对每个组合进行检验
  for(j in 1:ncol(combs)) {
    # 选择当前组合的数据
    subset <- larch[larch$SD %in% combs[,j], ]
    
    # 如果样本量太小，跳过
    if(nrow(subset) < 3) next
    
    # Shapiro-Wilk检验
    p_H <- shapiro.test(subset$H)$p.value
    p_D <- shapiro.test(subset$D)$p.value
    
    # 如果两个p值都大于0.05，保存结果
    results[[result_count]] <- list(
      sd_values = sort(combs[,j]),
      n = nrow(subset),
      p_value_H = p_H,
      p_value_D = p_D
    )
    result_count <- result_count + 1
  }
  # 打印进度
  cat("Completed testing combinations of size", i, "\n")
}




