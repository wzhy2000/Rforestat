# 问题一
library(forestat)
data(larch)
plot28 <- larch[larch$PLOT == "28", ]
plot71 <- larch[larch$PLOT == "71", ]
t.test(plot28$D, plot71$D, var.equal = TRUE)

# 问题二
t.test(plot28$D, plot71$D)

#问题三
t.test(plot28$D, plot71$D, alternative = "less")

# 问题四
var.test(plot28$D, plot71$D)

# 问题五
t.test(plot28$D, plot71$D, paired = TRUE)
