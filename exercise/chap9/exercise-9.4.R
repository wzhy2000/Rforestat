library(mgcv)
library(ggplot2)
library(splines)
library(forestat)
data("larch")
model_poly2 <- lm(CW ~ poly(D, 2), data = larch)
model_poly3 <- lm(CW ~ poly(D, 3), data = larch)
model_spline <- lm(CW ~ bs(D, knots = c(10, 20, 30)), data = larch)
model_loess <- loess(CW ~ D, data = larch)
model_gam <- gam(CW ~ s(D), data = larch)
larch$poly2 <- predict(model_poly2)
larch$poly3 <- predict(model_poly3)
larch$spline <- predict(model_spline)
larch$loess <- predict(model_loess)
larch$gam <- predict(model_gam)
ggplot(larch, aes(x = D)) +
  geom_point(aes(y = CW), color = "gray") +
  geom_line(aes(y = poly2), color = "blue", linetype = "dashed", size = 1) +
  geom_line(aes(y = spline), color = "red", size = 1) +
  geom_line(aes(y = loess), color = "purple", size = 1) +
  geom_line(aes(y = gam), color = "green", size = 1) +
  labs(title = "CW ~ D 多种拟合方法", y = "CW", x = "D")
summary(model_poly2)$r.squared
summary(model_poly3)$r.squared
summary(model_spline)$r.squared
summary(model_gam)$r.squared
AIC(model_poly2, model_poly3, model_spline, model_gam) 
