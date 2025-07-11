library(dplyr)
library(ggplot2)
library(forestat)
data("picea")
df <- picea %>% filter(PLOT1 %in% c(1, 2)) %>% select(PLOT1, D0)
df$PLOT1 <- factor(df$PLOT1)
var.test(D0 ~ PLOT1, data = df)
ggplot(df, aes(sample = D0)) + 
  stat_qq() + 
  stat_qq_line() + 
  facet_wrap(~PLOT1) + 
  labs(title = "Q-Q图分组比较")
t.test(D0 ~ PLOT1, data = df, var.equal = TRUE)
ggplot(df, aes(x = PLOT1, y = D0, fill = PLOT1)) +
  geom_boxplot() +
  labs(title = "两样地胸径分布", x = "样地", y = "胸径")
