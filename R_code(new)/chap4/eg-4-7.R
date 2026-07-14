library(forestat)
data("birch")
plot2 <- birch[which(birch$PLOT == 2), ]
t.test(plot2$H, mu = 12)

delta <- abs(mean(plot2$H) - 12)
sample.sd <- sd(plot2$H)
result.power <- power.t.test(n = length(plot2$H), delta = delta,
                               sd = sample.sd, sig.level = 0.05,
                               type = "one.sample", alternative = "two.sided")
print(result.power$power) 

delta_plan <- 1.0  # 研究前预设的最小重要差异
result.sample <- power.t.test(n = NULL, delta = delta_plan, 
                                sd = sd(plot2$H), sig.level = 0.05, power = 0.85,
                                type = "one.sample", alternative = "two.sided")
print(result.sample$n)