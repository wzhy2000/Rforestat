library(forestat)
library(nlme)
data(birch)

model.nls <- nls(H ~ a * log(CW) + b * D, data = birch, start = list(a = 1, b = 1))

deriv.formula <- deriv(~ a * log(CW) + b * D, 
                       c("a", "b"), 
                       function(CW, D, a, b){})

model.deriv <- nls(H ~ deriv.formula(CW, D, a, b), data = birch, start = list(a = 1, b = 1))

model.gnls <- gnls(H ~ log(a * CW) + b * D, data = birch, start = list(a = 1, b = 1),
                   weights = varPower(form = ~ fitted(.)))

summary(model.nls)
summary(model.deriv)
summary(model.gnls)


fittedvalues <- predict(model.nls)
predictions <- predictNLS(model.nls, alpha = 0.05, interval = "confidence")

coef(model.nls)
vcov(model.nls)

# 绘制残差图
pdf("Residuals.nls.pdf", width = 8, height = 8, family = "GB1")
par(mar = c(5, 5, 4, 2), mgp = c(3.5, 1, 0))
plot(fitted(model.nls), residuals(model.nls), xlab = "拟合值", 
     ylab = "残差", pch = 16, col = "black", cex = 1, 
     cex.lab = 2, cex.axis = 2)
abline(h = 0, col = "red")
dev.off()

profile.nls <- profile(model.nls)
pdf("nls.profile.pdf", width = 8, height = 6, family = "GB1")
par(mfrow = c(1, 2))
plot(profile.nls, cex.axis = 2, cex.lab = 2)
dev.off()



deriv3.formula <- deriv3(~ a * log(CW) + b * D, 
                         c("a", "b"), 
                         function(CW, D, a, b) NULL)

model.deriv3 <- nls(H ~ deriv3.formula(CW, D, a, b), data = birch, start = list(a = 1, b = 1))

rms.curv(model.deriv3)
