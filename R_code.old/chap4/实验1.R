library(forestat)
data(larch)

data_1 <- larch[which(larch$SD >= 400 & larch$SD <= 550), ]
data_2 <- larch[which(larch$SD >= 1140 & larch$SD <= 1150), ]

shapiro.test(data_1$H)
shapiro.test(data_2$H)

ks.test(data_1$H, data_2$H)

t.test(data_1$H, mu = 10)

library(EnvStats)
varTest(data_1$H, sigma.squared = 2.5)

t.test(data_1$H, data_2$H)

H_1 <- data_1$H + runif(length(data_1$H), min = 10e-6, max = 10e6)
D_1 <- data_1$D + runif(length(data_1$D), min = 10e-6, max = 10e6)
cor.test(H_1, D_1, method = "spearman")

data_1$plot <- factor(data_1$plot)
aov <- aov(data_1$H ~ data_1$plot)
summary(aov)
