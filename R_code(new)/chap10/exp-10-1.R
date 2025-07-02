library(forestat)
library(dplyr)
library(MASS)
library(nlme)

###########(1)数据集划分 ###################
data(birch)
pt <- unique(birch$PLOT)
hdo.list <- lapply(pt, function(p) {
  max(birch[birch$PLOT == p, "H"], na.rm = TRUE)
})
hdo.df <- data.frame(PLOT = pt, hdo = unlist(hdo.list))
data <- birch %>% left_join(hdo.df, by = "PLOT")
set.seed(123)
# 划分索引，70% 的数据用于训练
train.index <- sample(1:nrow(data), size = 0.7 * nrow(data))
# 创建训练集和测试集
train.data <- data[train.index, ]
test.data <- data[-train.index, ]

#############(2) 模型构建 ################

myInitial <- function(mCall, LHS, data, ...) {
  D <- data[["D"]]
  y <- data[["H"]]
  hdo <- data[["hdo"]]
  b1 <- max(y, na.rm = TRUE) / max(hdo, na.rm = TRUE) 
  b2 <- coef(lm(log(y) ~ log(hdo)))[2]
  b3 <- 1 / mean(D, na.rm = TRUE)
  b4 <- sd(y, na.rm = TRUE) / mean(y, na.rm = TRUE)
  value <- c(b1 = b1, b2 = b2, b3 = b3, b4 = b4)
  names(value) <- mCall[c("b1", "b2", "b3", "b4")]
  return(value)
}

mySelfStart <- selfStart(~ 1.3 + b1 * hdo^b2 * (1 - exp(-b3 * D))^b4, initial = myInitial, 
                         parameters = c("b1", "b2", "b3", "b4"))

start <- getInitial(H ~ mySelfStart(hdo = hdo, D = D, b1, b2, b3, b4), data = train.data)

deriv3.formula <- deriv3(~ 1.3 + b1 * hdo^b2 * (1 - exp(-b3 * D))^b4, c("b1", "b2", "b3", "b4"), function(hdo, D, b1, b2, b3, b4) NULL)

model.nls2 <- nls(H ~ deriv3.formula(hdo, D, b1, b2, b3, b4), data = train.data, 
                  start = list(b1 = start[1], b2 = start[2], b3 = start[3], b4 = start[4]))

model.gnls2 <- gnls(H ~ deriv3.formula(hdo, D, b1, b2, b3, b4), data = train.data, 
                    params = list(b1 ~ 1, b2 ~ 1, b3 ~ 1, b4 ~ 1),  # 指定参数结构
                    start = list(b1 = start[1], b2 = start[2], b3 = start[3], b4 = start[4]),
                    weights = varPower(form = ~ fitted(.)))


summary(model.nls2)
summary(model.gnls2)

rss.nls2 <- sum((fitted(model.nls2) - train.data$H)^2)
df.nls2 <- nrow(train.data) - length(coef(model.gnls2))
sqrt(rss.nls2 / df.nls2)

rss.gnls2 <- sum((fitted(model.gnls2) - train.data$H)^2)
df.gnls2 <- nrow(train.data) - length(coef(model.gnls2))
sqrt(rss.gnls2 / df.gnls2)

rss.gnls2 <- sum(residuals(model.gnls2, type = "response")^2)
rss.gnls2 <- sum(residuals(model.gnls2, type = "pearson")^2)
rss.gnls2 <- sum(residuals(model.gnls2, type = "normalized")^2)

sigma(model.gnls2)
df.gnls2 <- nrow(train.data) - length(coef(model.gnls2))
sqrt(deviance(model.gnls2)/1818)


############ (3)模型性能 ####################
FittingEvaluationIndex(fitted(model.nls2), train.data$H)
FittingEvaluationIndex(fitted(model.gnls2), train.data$H)

cat("AIC: ", "model.nls:", AIC(model.nls2), "model.gnls:", AIC(model.gnls2), "\n")
cat("BIC: ", "model.nls:", BIC(model.nls2), "model.gnls:", BIC(model.gnls2), "\n")


#############(4) 模型诊断 ####################

profile.nls2 <- profile(model.nls2)
pdf("exp.profile.pdf", width = 8, height = 8, family = "GB1")
par(mfrow = c(2, 2))
plot(profile.nls2, cex.lab = 2, cex.axis = 2)
dev.off()

rms.curv(model.nls2)

RSS <- sum(residuals(model.nls2)^2)
TSS <- sum((train.data$H - mean(train.data$H))^2)
pseudo_R2 <- 1 - (RSS / TSS)
pseudo_R2

############## (5) 模型性能 ######################
pre.nls2 <- predict(model.nls2, newdata = test.data)

FittingEvaluationIndex(pre.nls2, test.data$H)

############### (6) 可视化 ###############

pdf("9.9fit.plot.pdf", width = 8, height = 8, family = "GB1")
par(mar = c(5, 5, 4, 2), mgp = c(3.5, 1, 0))
plot(pre.nls2, test.data$H, xlab = "拟合树高(m)", ylab = "树高(m)", las = 1, 
     pch = 16, col = "black", cex = 1, 
     cex.lab = 2.5, cex.axis = 2.5)
dev.off()

# 训练集残差图
res.train <- residuals(model.nls2, type = "response")
pdf("9.10Residuals.train.pdf", width = 8, height = 8, family = "GB1")
par(mar = c(5, 5, 4, 2), mgp = c(3.5, 1, 0), mfrow = c(1, 1))
plot(fitted(model.nls2), res.train, xlab = "拟合值(m)", 
     ylab = "残差(m)", pch = 16, col = "black", cex = 1, 
     cex.lab = 2.5, cex.axis = 2.5)
abline(h = 0, col = "red")
dev.off()


# 测试集残差图
res.test <- test.data$H - pre.nls2
pdf("9.11Residuals.test.pdf", width = 8, height = 8, family = "GB1")
par(mar = c(5, 5, 4, 2), mgp = c(3.5, 1, 0))
plot(pre.nls2, res.test, xlab = "拟合值(m)", 
     ylab = "残差(m)", pch = 16, col = "black", cex = 1, 
     cex.lab = 2.5, cex.axis = 2.5)
abline(h = 0, col = "red")
dev.off()

