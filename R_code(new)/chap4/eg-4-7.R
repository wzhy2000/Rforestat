library(forestat)
data("birch")
plot2 <- birch[which(birch$PLOT == 2), ]
t.test(plot2$H, mu = 12)

effect_size <- abs(mean(plot2$H) - 12) / sd(plot2$H)
result.power <- power.t.test(n = length(plot2$H), delta = effect_size, 
                            sd = sd(plot2$H), sig.level = 0.05,
                            type = "one.sample", alternative = "two.sided")
print(result.power$power)

result.sample <- power.t.test(n = NULL, delta = effect_size, 
                              sd = sd(plot2$H), sig.level = 0.05, power = 0.85,
                              type = "one.sample", alternative = "two.sided")
print(result.sample$n)