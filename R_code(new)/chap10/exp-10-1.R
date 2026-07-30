library(forestat)
library(dplyr)
library(MASS)
library(nlme)

###########(1)数据集划分 ###################
set.seed(123)
plot.id <- unique(birch$PLOT)
train.plot <- sample(
  plot.id, size = floor(0.7 * length(plot.id))
)
hmax.df <- birch %>%
  group_by(PLOT) %>%
  summarise(hmax = max(H, na.rm = TRUE), .groups = "drop")
model.data <- birch %>% left_join(hmax.df, by = "PLOT")
train.data <- model.data[model.data$PLOT %in% train.plot, ]
test.data <- model.data[!model.data$PLOT %in% train.plot, ]
#############(2) 模型构建 ################

myInitial <- function(mCall, LHS, data, ...) {
  D <- data[["D"]]
  y <- data[["H"]]
  hmax <- data[["hmax"]]
  ok <- is.finite(D) & is.finite(y) & is.finite(hmax) &
    D > 0 & y > 0 & hmax > 0
  D <- D[ok]
  y <- y[ok]
  hmax <- hmax[ok]
  b1 <- max(y) / max(hmax)
  b2 <- unname(coef(lm(log(y) ~ log(hmax)))[2])
  b3 <- 1 / mean(D, na.rm = TRUE)
  b4 <- sd(y, na.rm = TRUE) / mean(y, na.rm = TRUE)
  value <- c(b1 = b1, b2 = b2, b3 = b3, b4 = b4)
  names(value) <- mCall[c("b1", "b2", "b3", "b4")]
  return(value)
}
mySelfStart <- selfStart(~ 1.3 + b1 * hmax^b2 * (1 - exp(-b3 * D))^b4,
                         initial = myInitial, parameters = c("b1", "b2", "b3", "b4"))
start <- getInitial(H ~ mySelfStart(hmax = hmax, D = D, b1, b2, b3, b4),
                    data = train.data)
start


deriv3.formula <- deriv3(~ 1.3 + b1 * hmax^b2 * (1 - exp(-b3 * D))^b4,
                         c("b1", "b2", "b3", "b4"),
                         function(hmax, D, b1, b2, b3, b4) NULL)
model.nls2 <- nls(H ~ deriv3.formula(hmax, D, b1, b2, b3, b4), data = train.data,
                  start = list(b1 = unname(start[1]), b2 = unname(start[2]),
                               b3 = unname(start[3]), b4 = unname(start[4])))
model.gnls2 <- nlme::gnls(
  H ~ deriv3.formula(hmax, D, b1, b2, b3, b4), data = train.data,
  params = list(b1 ~ 1, b2 ~ 1, b3 ~ 1, b4 ~ 1),
  start = c(b1 = unname(start[1]), b2 = unname(start[2]),
            b3 = unname(start[3]), b4 = unname(start[4])),
  weights = nlme::varPower(form = ~ fitted(.)))


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
par(mar = c(5, 6, 4, 2), mgp = c(3.5, 1, 0))
plot(pre.nls2, test.data$H, xlab = "预测树高(m)", ylab = "观测树高(m)", las = 1, 
     pch = 16, col = "black", cex = 1, 
     cex.lab = 2.5, cex.axis = 2.5)
dev.off()

# 训练集残差图
res.train <- residuals(model.nls2, type = "response")
pdf("9.10Residuals.train.pdf", width = 8, height = 8, family = "GB1")
par(mar = c(5, 6, 4, 2), mgp = c(3.5, 1, 0), mfrow = c(1, 1))
plot(fitted(model.nls2), res.train, xlab = "拟合值(m)", 
     ylab = "残差(m)", pch = 16, col = "black", cex = 1, 
     cex.lab = 2.5, cex.axis = 2.5)
abline(h = 0, col = "red")
dev.off()


# 测试集残差图
res.test <- test.data$H - pre.nls2
pdf("9.11Residuals.test.pdf", width = 8, height = 8, family = "GB1")
par(mar = c(5, 6, 4, 2), mgp = c(3.5, 1, 0))
plot(pre.nls2, res.test, xlab = "拟合值(m)", 
     ylab = "残差(m)", pch = 16, col = "black", cex = 1, 
     cex.lab = 2.5, cex.axis = 2.5)
abline(h = 0, col = "red")
dev.off()

