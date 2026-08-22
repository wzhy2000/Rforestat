# 习题 10.3：胸径-树高关系的线性与幂函数比较

library(minpack.lm)
library(ggplot2)
data("picea", package = "forestat")
d <- subset(picea, complete.cases(D0, LH, PLOT1) & D0 > 0 & LH > 0)
d$PLOT1 <- factor(d$PLOT1)

# （1）D0 为实测胸径，LH 为激光雷达树高；核对范围。
print(summary(d[c("D0", "LH")]))

# （2）按题干以 D0 为响应、LH 为预测变量。
linear_model <- lm(D0 ~ LH, data = d)
log_start <- lm(log(D0) ~ log(LH), data = d)
power_start <- list(a = exp(coef(log_start)[1]), b = coef(log_start)[2])
power_model <- nlsLM(D0 ~ a * LH^b, data = d, start = power_start, lower = c(0, -Inf))
print(summary(linear_model))
print(summary(power_model))
print(AIC(linear_model, power_model))

# （3）按 PLOT1 五折分组验证。
set.seed(123)
plots <- sample(levels(d$PLOT1))
plot_fold <- setNames(rep(1:5, length.out = length(plots)), plots)
fold_id <- unname(plot_fold[as.character(d$PLOT1)])
cv <- do.call(rbind, lapply(1:5, function(fold) {
  train <- d[fold_id != fold, ]
  test <- d[fold_id == fold, ]
  ml <- lm(D0 ~ LH, data = train)
  ls <- lm(log(D0) ~ log(LH), data = train)
  mp <- nlsLM(D0 ~ a * LH^b, data = train, start = list(a = exp(coef(ls)[1]), b = coef(ls)[2]), lower = c(0, -Inf))
  data.frame(observed = test$D0, linear = predict(ml, newdata = test), power = predict(mp, newdata = test))
}))
metrics <- function(predicted) c(
  RMSE = sqrt(mean((cv$observed - predicted)^2)),
  MAE = mean(abs(cv$observed - predicted)),
  R2 = 1 - sum((cv$observed - predicted)^2) / sum((cv$observed - mean(cv$observed))^2)
)
print(rbind(linear = metrics(cv$linear), power = metrics(cv$power)))

grid <- data.frame(LH = seq(min(d$LH), max(d$LH), length.out = 200))
curves <- rbind(
  data.frame(grid, D0 = predict(linear_model, newdata = grid), model = "linear"),
  data.frame(grid, D0 = predict(power_model, newdata = grid), model = "power")
)
if (!interactive()) grDevices::cairo_pdf(tempfile("exercise-10.3-", fileext = ".pdf"), width = 8, height = 5)
print(ggplot(d, aes(LH, D0)) + geom_point(alpha = 0.25) + geom_line(data = curves, aes(colour = model), linewidth = 1) + theme_minimal())
if (!interactive()) dev.off()
cat("线性模型给恒定边际变化，幂指数 b 表示弹性；两种形式都不应任意外推。\n")
