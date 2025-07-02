library(nlme)
library("forestat")
data(birch)
colname = c('CW','D','SD','CLR','PLOT')#选择需要的列
data = birch[,colname]
head(data, n=3)


# 构建混合效应模型 CW_model
model.CW.Pt = nlme(CW ~ (phi1 + phi2 * CLR)/(1 + phi3 * exp(-(phi4 + phi5 * SD) * D)), 
                   data = data, 
                   fixed = (phi1 + phi2 + phi3 + phi4 + phi5 ~ 1), 
                   random = list(PLOT = phi1 ~ 1), 
                   start = c(phi1 = 4.396, phi2 = 1.840, phi3 = 2.231, phi4 = 0.1356, phi5 = -0.00002093))

# 查看结果
summary(model.CW.Pt)

names(model.CW.Pt)

ranef.CW <- ranef(model.CW.Pt)
head(ranef.CW, n = 3)

data$fit.CW <-  fitted(model.CW.Pt)
names(data)

# 计算模型的 R 平方值
sst <- sum((data$CW - mean(data$CW))^2)
ssr <- sum((data$fit.CW - mean(data$CW))^2)
rsquared <- ssr / sst                      # R 平方值
rsquared

# 绘制预测值和观测值的散点图
pdf("图10.1a.pdf", width = 8, height = 6, family = "GB1")
par(mar = c(5, 5.5, 4, 2), mgp = c(3.5, 1, 0))
plot(data$fit.CW, data$CW, xlab = "拟合冠幅(cm)", ylab = "冠幅(cm)", col = "black", cex.axis = 2.5, cex.lab = 2.5)
dev.off()

# 绘制模型的拟合值与残差图
pdf("图10.1b.pdf", width = 8, height = 6, family = "GB1")
par(mar = c(5, 5.5, 4, 2), mgp = c(3.5, 1, 0))
plot(fitted(model.CW.Pt), residuals(model.CW.Pt), xlab = "拟合冠幅(cm)", ylab = "残差(cm)",  cex.axis = 2.5, cex.lab = 2.5)
abline(h = 0, col = "red")
dev.off()

# 使用varPower处理异方差
model.CW.Pt.varPower <- nlme(CW ~ (phi1 + phi2 * CLR) / (1 + phi3 * exp(-(phi4 + phi5 * SD) * D)), data = data, weights = varPower(form = ~ SD), fixed = (phi1 + phi2 + phi3 + phi4 + phi5 ~ 1), random = list(PLOT = pdDiag(phi1 ~ 1)), start = c(phi1 = 4.396, phi2 = 1.840, phi3 = 2.231, phi4 = 0.1356, phi5 = -0.00002093))
pdf("图10.1c.pdf", width = 8, height = 6, family = "GB1")
par(mar = c(5, 5.5, 4, 2), mgp = c(3.5, 1, 0))
plot(fitted(model.CW.Pt.varPower ), residuals(model.CW.Pt.varPower), xlab = "拟合冠幅(cm)", ylab = "残差(cm)",  cex.axis = 2.5, cex.lab = 2.5)
abline(h = 0, col = "red")
dev.off()

#利用nls构建
model.CW.nls = nls(CW ~ (phi1 + phi2 * CLR)/(1 + phi3 * exp(-(phi4 + phi5 * SD) * D)), data = data, start = c(phi1 = 4.396, phi2 = 1.840, phi3 = 2.231, phi4 = 0.1356, phi5 = -0.00002093))
summary(model.CW.nls)

predict.nls = predict(model.CW.nls)
sst <- sum((data$CW - mean(data$CW))^2)
ssr <- sum((predict.nls - mean(data$CW))^2)
nls.rsquared <- ssr / sst              # R 平方值
nls.rsquared

# 通过方差分析对 nlme 模型和 nls 模型进行比较
anova(model.CW.Pt, model.CW.nls)


#模型预测
data$predict.CW <- predict(model.CW.Pt, data, level = 0)
sst <- sum((data$CW - mean(data$CW))^2)
ssr <- sum((data$predict.CW - mean(data$CW))^2)
rsquared <- ssr / sst
rsquared

data$predict.CW1 <- predict(model.CW.Pt, data, level = 1)
sst <- sum((data$CW - mean(data$CW))^2)
ssr <- sum((data$predict.CW1 - mean(data$CW))^2)
rsquared <- ssr / sst
rsquared


