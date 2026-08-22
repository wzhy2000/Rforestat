# 习题 11.3：落叶松冠幅幂函数的两水平非线性混合效应模型

library(nlme)
data("larch", package = "forestat")
d <- subset(larch, complete.cases(CW, D, H, PLOT, AGE.GROUP) & CW > 0 & D > 0 & H > 0)
d$PLOT <- factor(d$PLOT)
d$AGE.GROUP <- factor(d$AGE.GROUP)

# 使用 exp(loga) 保证尺度参数 a 为正。
start_model <- lm(log(CW) ~ log(D) + log(H), data = d)
start_values <- c(
  loga = unname(coef(start_model)[1]),
  b = unname(coef(start_model)[2]),
  c = unname(coef(start_model)[3])
)

age_plot_combinations <- unique(d[c("AGE.GROUP", "PLOT")])
plots_per_age_group <- table(age_plot_combinations$AGE.GROUP)
age_groups_per_plot <- table(age_plot_combinations$PLOT)
cat("AGE.GROUP 水平数：", nlevels(d$AGE.GROUP), "；PLOT 编号数：", nlevels(d$PLOT), "\n")
cat("实际 AGE.GROUP/PLOT 组合数：", nrow(age_plot_combinations), "\n")
print(plots_per_age_group)
if (any(age_groups_per_plot > 1L)) {
  cat("在多个 AGE.GROUP 中重复使用的 PLOT 编号：", paste(names(age_groups_per_plot)[age_groups_per_plot > 1L], collapse = ", "), "\n")
  cat("AGE.GROUP/PLOT 将这些编号解释为不同的嵌套组合。\n")
}

# （1）无随机效应与 PLOT 随机 log(a) 模型。
no_random_model <- gnls(
  CW ~ exp(loga) * D^b * H^c,
  data = d, start = start_values
)
plot_random_model <- nlme(
  CW ~ exp(loga) * D^b * H^c,
  data = d,
  fixed = loga + b + c ~ 1,
  random = loga ~ 1 | PLOT,
  start = start_values, method = "ML",
  control = nlmeControl(maxIter = 200, pnlsMaxIter = 50, msMaxIter = 200)
)

sample_r2 <- function(observed, predicted) {
  1 - sum((observed - predicted)^2) / sum((observed - mean(observed))^2)
}
comparison <- data.frame(
  model = c("无随机效应", "PLOT 随机 log(a)"),
  AIC = c(AIC(no_random_model), AIC(plot_random_model)),
  residual_SD = c(sigma(no_random_model), sigma(plot_random_model)),
  sample_R2 = c(
    sample_r2(d$CW, fitted(no_random_model)),
    sample_r2(d$CW, predict(plot_random_model, level = 1))
  ),
  PLOT_random_SD = c(NA, as.numeric(VarCorr(plot_random_model)["loga", "StdDev"]))
)
print(comparison, row.names = FALSE)

# （2）AGE.GROUP/PLOT 嵌套两水平随机截距模型。
nested_model <- nlme(
  CW ~ exp(loga) * D^b * H^c,
  data = d,
  fixed = loga + b + c ~ 1,
  random = loga ~ 1 | AGE.GROUP / PLOT,
  start = start_values, method = "ML",
  control = nlmeControl(maxIter = 200, pnlsMaxIter = 50, msMaxIter = 200)
)
cat("嵌套模型固定效应：\n"); print(fixef(nested_model))
cat("嵌套模型 AIC 和样本内条件 R2：\n")
print(c(AIC = AIC(nested_model), conditional_R2 = sample_r2(d$CW, predict(nested_model, level = 2))))

# （3）报告两个层次的方差分量和随机效应条件预测排序图。
cat("AGE.GROUP 与 PLOT %in% AGE.GROUP 方差分量：\n")
print(VarCorr(nested_model))
nested_random_effects <- ranef(nested_model)
age_group_effects <- sort(nested_random_effects$AGE.GROUP[, "loga"])
plot_effects <- sort(nested_random_effects$PLOT[, "loga"])

if (!interactive()) grDevices::cairo_pdf(tempfile("exercise-11.3-", fileext = ".pdf"), width = 10, height = 7)
par(mfrow = c(1, 2), mar = c(4, 8, 3, 1))
dotchart(age_group_effects, main = "AGE.GROUP 随机效应", xlab = "log(a) 条件预测偏离", pch = 16)
dotchart(plot_effects, main = "PLOT %in% AGE.GROUP 随机效应", xlab = "log(a) 条件预测偏离", pch = 16, cex = 0.6)
abline(v = 0, lty = 2, col = "red")
if (!interactive()) dev.off()
cat("随机效应表示样本内分组异质性，不能自动归因于未测量环境因素。\n")
