library(splines)
library(forestat)
data(larch)

plot.model <- function(x, y, x.seq, y.preds){
  plot(x, y, cex = 0.5, col = "darkgray", xlab = "D", ylab = "CW")
  lines(x.seq, y.preds$fit, lwd = 2, col = "blue")
  lines(x.seq, y.preds$fit + 2 * y.preds$se.fit, lty = "dashed")
  lines(x.seq, y.preds$fit - 2 * y.preds$se.fit, lty = "dashed")
}


# 多项式回归模型
model.p3 <- lm(CW ~ poly(D, 3), data = larch)
summary(model.p3)
cat(AIC(model.p3), BIC(model.p3))
FittingEvaluationIndex(predict(model.p3, newdata = larch), larch$CW)

model.p3a <- lm(CW ~ D + I(D^2) + I(D^3), data = larch)

model.p3b <- lm(CW ~ cbind(D, D^2, D^3), data = larch)

xlims <- range(larch$D)
x.seq <- seq(from = xlims[1], to = xlims[2])
y.preds <- predict(model.p3, newdata = list(D = x.seq), se = TRUE)

plot.model(larch$D, larch$CW, x.seq, y.preds)

model.p1 <- lm(CW ~ D, data = larch)
model.p3 <- lm(CW ~ poly(D, 3, raw = T), data = larch)
model.p4 <- lm(CW ~ poly(D, 4, raw = T), data = larch)

anova(model.p1, model.p3, model.p4)


# 回归样条
model.bs <- lm(CW ~ bs(D, df = 6), data = larch)
summary(model.bs)
y.preds <- predict(model.bs, newdata = list(D = x.seq), se = TRUE)

cat(AIC(model.bs), BIC(model.bs))
FittingEvaluationIndex(predict(model.bs, newdata = larch), larch$CW)

plot.knots <- function(model, splines){
  actual_knots <- attr(splines, "knots")
  y_knots <- predict(model, newdata = data.frame(D = actual_knots))
  points(actual_knots, y_knots, col = "red", pch = 19,cex = 0.5)
  for (i in 1:length(actual_knots)) {
    abline(v = actual_knots[i], lty = 2, col = "red")
  }
}

plot.model(larch$D, larch$CW, x.seq, y.preds)
plot.knots(model.bs, model.bs$model$bs)

# 自然样条
model.ns <- lm(CW ~ ns(D, df = 4), data = larch)
summary(model.ns)
y.preds <- predict(model.ns, newdata = list(D = x.seq), se = TRUE)

cat(AIC(model.ns), BIC(model.ns))
FittingEvaluationIndex(predict(model.ns, newdata = larch), larch$CW)
plot.model(larch$D, larch$CW, x.seq, y.preds)
plot.knots(model.ns, model.ns$model$ns)


# 光滑样条
model.ss5 <- smooth.spline(larch$D, larch$CW, df = 5)
model.ss <- smooth.spline(larch$D, larch$CW, cv = TRUE)

FittingEvaluationIndex(predict(model.ss5, larch$D)$y, larch$CW)
FittingEvaluationIndex(predict(model.ss, larch$D)$y, larch$CW)

y.preds <- list(x = model.ss5$x, y = model.ss5$yin, fit = model.ss5$y )
res <- (model.ss5$yin - model.ss5$y) / (1 - model.ss5$lev)
sigma <- sqrt(var(res))
y.preds$se.fit <- sigma * sqrt(model.ss5$lev)
plot.model(larch$D, larch$CW, y.preds$x, y.preds)

# 局部回归
model.lo1 <- loess(CW ~ D, span = 0.1, data = larch)
model.lo5 <- loess(CW ~ D, span = 0.5, data = larch)

FittingEvaluationIndex(predict(model.lo1, newdata = larch), larch$CW)
FittingEvaluationIndex(predict(model.lo5, newdata = larch), larch$CW)

y.preds <- predict(model.lo1, data.frame(D = x.seq), se = TRUE)
plot.model(larch$D, larch$CW, x.seq, y.preds)

y.preds <- predict(model.lo5, data.frame(D = x.seq), se = TRUE)
plot.model(larch$D, larch$CW, x.seq, y.preds)
