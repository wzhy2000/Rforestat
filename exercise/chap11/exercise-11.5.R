# 习题 11.5：mite Shannon 多样性沿水分梯度的非线性变化

library(vegan)
library(minpack.lm)
library(nlme)

data("mite", package = "vegan")
data("mite.env", package = "vegan")

shannon <- diversity(mite, index = "shannon")
d <- data.frame(
  Shannon = shannon,
  WatrCont = mite.env$WatrCont,
  Substrate = factor(mite.env$Substrate)
)
d <- subset(d, complete.cases(d) & WatrCont > 0)

cat("分析样方数：", nrow(d), "；Substrate 水平数：", nlevels(d$Substrate), "\n")
cat("各 Substrate 的样本量：\n")
print(table(d$Substrate))

# （1）比较幂函数和 log-logistic 固定效应模型。
power_model <- nlsLM(
  Shannon ~ a * WatrCont^b,
  data = d,
  start = c(a = 10, b = -0.25),
  control = nls.lm.control(maxiter = 500)
)

# 三参数 log-logistic：upper 为上限，ed50 为曲线中点，slope 控制陡峭程度。
log_logistic_formula <- Shannon ~
  exp(log_upper) /
  (1 + exp(slope * (log(WatrCont) - log_ed50)))
log_logistic_model <- nlsLM(
  log_logistic_formula,
  data = d,
  start = c(log_upper = log(2.2), slope = 4, log_ed50 = log(700)),
  lower = c(log_upper = -Inf, slope = 0, log_ed50 = log(min(d$WatrCont))),
  upper = c(log_upper = Inf, slope = Inf, log_ed50 = log(max(d$WatrCont)))
)

sample_r2 <- function(observed, predicted) {
  1 - sum((observed - predicted)^2) / sum((observed - mean(observed))^2)
}

fixed_comparison <- data.frame(
  model = c("幂函数", "log-logistic"),
  AIC = c(AIC(power_model), AIC(log_logistic_model)),
  residual_SD = c(sigma(power_model), sigma(log_logistic_model)),
  sample_R2 = c(
    sample_r2(d$Shannon, predict(power_model)),
    sample_r2(d$Shannon, predict(log_logistic_model))
  )
)
print(fixed_comparison, row.names = FALSE)
cat("幂函数参数：\n")
print(coef(power_model))
cat("log-logistic 参数：\n")
print(coef(log_logistic_model))

# （2）在 log-logistic 的 slope 参数上加入 Substrate 随机效应。
# 部分基质组样本很少，因此该模型主要用于演示分组曲线，方差估计须谨慎解释。
mixed_log_logistic <- nlme(
  log_logistic_formula,
  data = d,
  fixed = log_upper + slope + log_ed50 ~ 1,
  random = slope ~ 1 | Substrate,
  start = coef(log_logistic_model),
  method = "ML",
  control = nlmeControl(maxIter = 300, pnlsMaxIter = 80, msMaxIter = 300)
)

cat("带 Substrate 随机 slope 的模型：\n")
print(fixef(mixed_log_logistic))
print(VarCorr(mixed_log_logistic))
cat("Substrate 的 slope 随机效应条件预测值：\n")
print(ranef(mixed_log_logistic))

mixed_summary <- data.frame(
  model = "log-logistic + Substrate 随机 slope",
  AIC = AIC(mixed_log_logistic),
  residual_SD = sigma(mixed_log_logistic),
  sample_R2 = sample_r2(d$Shannon, predict(mixed_log_logistic, level = 1))
)
print(mixed_summary, row.names = FALSE)

# （3）在相同 WatrCont 网格上比较总体曲线和各 Substrate 条件曲线。
x_grid <- exp(seq(log(min(d$WatrCont)), log(max(d$WatrCont)), length.out = 200))
fixed_grid <- data.frame(WatrCont = x_grid)
fixed_grid$power <- predict(power_model, newdata = fixed_grid)
fixed_grid$log_logistic <- predict(log_logistic_model, newdata = fixed_grid)

substrate_grid <- expand.grid(
  WatrCont = x_grid,
  Substrate = levels(d$Substrate),
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)
substrate_grid$Substrate <- factor(substrate_grid$Substrate, levels = levels(d$Substrate))
substrate_grid$conditional <- predict(
  mixed_log_logistic,
  newdata = substrate_grid,
  level = 1
)

if (!interactive()) {
  grDevices::cairo_pdf(tempfile("exercise-11.5-", fileext = ".pdf"), width = 10, height = 5)
}
old_par <- par(mfrow = c(1, 2))
plot(
  d$WatrCont, d$Shannon, log = "x", pch = 16, col = "grey50",
  xlab = "WatrCont", ylab = "Shannon 多样性",
  main = "固定效应候选曲线"
)
lines(fixed_grid$WatrCont, fixed_grid$power, col = "steelblue", lwd = 2)
lines(fixed_grid$WatrCont, fixed_grid$log_logistic, col = "firebrick", lwd = 2)
legend("topright", c("幂函数", "log-logistic"), col = c("steelblue", "firebrick"), lwd = 2)

group_colours <- grDevices::hcl.colors(nlevels(d$Substrate), "Dark 3")
plot(
  d$WatrCont, d$Shannon, log = "x", pch = 16, col = "grey75",
  xlab = "WatrCont", ylab = "Shannon 多样性",
  main = "Substrate 条件曲线"
)
for (i in seq_along(levels(d$Substrate))) {
  group_name <- levels(d$Substrate)[i]
  one_group <- substrate_grid[substrate_grid$Substrate == group_name, ]
  lines(one_group$WatrCont, one_group$conditional, col = group_colours[i], lwd = 1.5)
}
legend("topright", levels(d$Substrate), col = group_colours, lwd = 1.5, cex = 0.75)
par(old_par)
if (!interactive()) dev.off()

cat(
  "模型比较仅说明本样本内的拟合表现；Substrate 小样本组的条件曲线和随机效应",
  "不宜作稳定的总体推断。\n",
  sep = ""
)
