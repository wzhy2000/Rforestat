library(forestat)
data(larch)
attach(larch)
fit1 = lm(CW ~ poly(D, 3), data = larch)
summary(fit1)
fit1_pre <- predict(fit1)

library(splines)
fit2 = lm(CW ~ bs(D, df = 6), data = larch)
summary(fit2)
fit2_pre <- predict(fit2)

fit3 = lm(CW ~ ns(D, df = 4), data = larch)
summary(fit3)
fit3_pre <- predict(fit3)

fit4 = smooth.spline(D, CW, cv = TRUE)
print(fit4)
fit4_pre <- predict(fit4)

fit5.1 = loess(CW ~ D,span = .1, data = larch)
fit5.2 = loess(CW ~ D,span = .5, data = larch)
summary(fit5.2)
fit5_pre <- predict(fit5.2)
