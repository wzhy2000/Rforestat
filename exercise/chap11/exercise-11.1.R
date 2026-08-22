# 习题 11.1：土壤 CO2 通量的 Logistic 非线性混合效应模型

library(fortedata)
library(minpack.lm)
library(nlme)

raw_data <- fd_soil_respiration()
needed <- c("soil_co2_efflux", "soil_temp", "vwc", "plot", "date")
d <- raw_data[complete.cases(raw_data[needed]) & raw_data$soil_co2_efflux > 0, needed]
d$log_efflux <- with(d, log(soil_co2_efflux))
d$plot <- factor(d$plot)
d <- d[order(d$plot, d$date), ]

cat("fortedata 版本：", as.character(packageVersion("fortedata")), "\n")
cat("分析记录：", nrow(d), "；样地数：", nlevels(d$plot), "；日期数：", length(unique(d$date)), "\n")
cat("因缺失或非正通量排除：", nrow(raw_data) - nrow(d), "条记录。\n")
print(summary(d[c("log_efflux", "soil_temp", "vwc")]))
print(summary(table(d$plot)))

# （1）mu = A / {1 + exp[-(b0 + b1*soil_temp + b2*vwc)]}。
# 先拟合无随机效应模型，获得稳定的 NLME 起始值。
mean_formula <- log_efflux ~ A / (1 + exp(-(b0 + b1 * soil_temp + b2 * vwc)))
start_values <- list(A = max(d$log_efflux) + 0.5, b0 = -2, b1 = 0.1, b2 = 0)
fixed_model <- nlsLM(
  mean_formula, data = d, start = start_values,
  lower = c(A = 0, b0 = -Inf, b1 = -Inf, b2 = -Inf),
  control = nls.lm.control(maxiter = 500)
)

mixed_model <- nlme(
  mean_formula, data = d,
  fixed = A + b0 + b1 + b2 ~ 1,
  random = A ~ 1 | plot,
  start = coef(fixed_model), method = "ML",
  control = nlmeControl(maxIter = 200, pnlsMaxIter = 50, msMaxIter = 200)
)
cat("固定效应估计：\n"); print(fixef(mixed_model))
cat("随机效应和残差标准差：\n"); print(VarCorr(mixed_model))

# （2）在同一批观测和同一均值函数下比较模型拟合。
fit_comparison <- data.frame(
  model = c("无随机效应", "plot 随机 A"),
  AIC = c(AIC(fixed_model), AIC(mixed_model)),
  residual_SD = c(sigma(fixed_model), sigma(mixed_model))
)
print(fit_comparison, row.names = FALSE)
cat("plot 仅有", nlevels(d$plot), "个水平，随机效应标准差应作为描述性估计谨慎解释。\n")

# （3）标准化残差与拟合值、温度和水分的关系。
d$fitted <- fitted(mixed_model)
d$normalized_residual <- residuals(mixed_model, type = "normalized")
if (!interactive()) grDevices::cairo_pdf(tempfile("exercise-11.1-", fileext = ".pdf"), width = 9, height = 7)
par(mfrow = c(2, 2))
plot(d$fitted, d$normalized_residual, pch = 16, cex = 0.45, xlab = "拟合值", ylab = "标准化残差")
abline(h = 0, lty = 2, col = "red")
plot(d$soil_temp, d$normalized_residual, pch = 16, cex = 0.45, xlab = "土壤温度", ylab = "标准化残差")
abline(h = 0, lty = 2, col = "red")
plot(d$vwc, d$normalized_residual, pch = 16, cex = 0.45, xlab = "体积含水量 (%)", ylab = "标准化残差")
abline(h = 0, lty = 2, col = "red")
qqnorm(d$normalized_residual, pch = 16, cex = 0.45); qqline(d$normalized_residual, col = "red")
if (!interactive()) dev.off()

# 原始数据在同一 plot-date 下包含多个 subplot/run，直接使用
# corAR1(form = ~date | plot) 会因组内时间值重复而失效。
duplicate_plot_dates <- sum(duplicated(d[c("plot", "date")]))
cat("原始数据中重复的 plot-date 行数：", duplicate_plot_dates, "\n")

# 为演示题目指定的日期相关结构，先汇总为唯一的 plot-date 均值，
# 再在相同汇总数据上比较独立残差与 AR(1) 候选模型。
date_data <- aggregate(
  cbind(log_efflux, soil_temp, vwc) ~ plot + date,
  data = d, FUN = mean
)
date_data <- date_data[order(date_data$plot, date_data$date), ]
stopifnot(!anyDuplicated(date_data[c("plot", "date")]))

date_independent <- nlme(
  mean_formula, data = date_data,
  fixed = A + b0 + b1 + b2 ~ 1,
  random = A ~ 1 | plot,
  start = fixef(mixed_model), method = "ML",
  control = nlmeControl(maxIter = 200, pnlsMaxIter = 50, msMaxIter = 200)
)
date_ar1 <- update(
  date_independent,
  correlation = corAR1(form = ~as.numeric(date) | plot)
)
cat("plot-date 汇总数据的相关结构比较：\n")
print(AIC(date_independent, date_ar1))
ar1_phi <- coef(date_ar1$modelStruct$corStruct, unconstrained = FALSE)
cat("AR(1) 残差相关参数：", unname(ar1_phi), "\n")
cat("AR(1) 结果针对 plot-date 汇总序列，不应与原始逐次测量模型的 AIC 直接比较。\n")
