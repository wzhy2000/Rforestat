# 习题 9.2：CO2 重复测量的 loess 描述与 GAMM

library(mgcv)
library(ggplot2)
data("CO2", package = "datasets")
CO2$group <- interaction(CO2$Type, CO2$Treatment, drop = TRUE)

# （1）绘制每株原始轨迹和 Type x Treatment 分组 loess 描述曲线。
if (!interactive()) grDevices::cairo_pdf(tempfile("exercise-9.2-", fileext = ".pdf"), width = 9, height = 6)
print(
  ggplot(CO2, aes(x = conc, y = uptake, group = Plant, colour = group)) +
    geom_line(alpha = 0.45) + geom_point(alpha = 0.6) +
    geom_smooth(aes(group = group), method = "loess", se = FALSE, linewidth = 1.1) +
    labs(title = "各 Plant 轨迹及分组 loess 描述曲线", colour = "Type x Treatment") +
    theme_minimal()
)

# （2）用 Plant 随机截距处理株内相关，各组使用低复杂度浓度平滑。
model <- gamm(
  uptake ~ group + s(conc, by = group, k = 5),
  random = list(Plant = ~1),
  data = CO2,
  method = "REML"
)
print(summary(model$gam))
print(summary(model$lme))

# （3）在统一浓度网格上输出各组曲线及点态 95% 区间。
group_levels <- levels(CO2$group)
prediction_data <- do.call(rbind, lapply(group_levels, function(g) {
  grid <- data.frame(
    conc = seq(min(CO2$conc), max(CO2$conc), length.out = 200),
    group = factor(g, levels = group_levels)
  )
  p <- predict(model$gam, newdata = grid, type = "link", se.fit = TRUE)
  transform(grid, fit = p$fit, lower = p$fit - 1.96 * p$se.fit, upper = p$fit + 1.96 * p$se.fit)
}))
print(
  ggplot(prediction_data, aes(conc, fit, colour = group, fill = group)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.12, colour = NA) +
    geom_line(linewidth = 1) +
    labs(title = "GAMM 分组响应曲线", y = "预测 uptake") +
    theme_minimal()
)

# （4）用 lpmatrix 数值导数及其区间识别“增长明显放缓”范围。
derivative_results <- lapply(group_levels, function(g) {
  x <- seq(min(CO2$conc), max(CO2$conc), length.out = 200)
  h <- 0.01 * diff(range(CO2$conc))
  plus <- data.frame(conc = x + h, group = factor(g, levels = group_levels))
  minus <- data.frame(conc = x - h, group = factor(g, levels = group_levels))
  derivative_matrix <- (predict(model$gam, plus, type = "lpmatrix") - predict(model$gam, minus, type = "lpmatrix")) / (2 * h)
  estimate <- as.numeric(derivative_matrix %*% coef(model$gam))
  se <- sqrt(rowSums((derivative_matrix %*% vcov(model$gam)) * derivative_matrix))
  data.frame(group = g, conc = x, derivative = estimate, lower = estimate - 1.96 * se, upper = estimate + 1.96 * se)
})
derivative_results <- do.call(rbind, derivative_results)
slow_ranges <- do.call(rbind, lapply(split(derivative_results, derivative_results$group), function(z) {
  slow <- z$lower <= 0 & z$upper >= 0
  data.frame(group = z$group[1], from = if (any(slow)) min(z$conc[slow]) else NA, to = if (any(slow)) max(z$conc[slow]) else NA)
}))
print(slow_ranges)
if (!interactive()) dev.off()
cat("导数区间包含 0 只表示增长已难以区分于 0，应称增长明显放缓，不等同数学拐点。\n")
