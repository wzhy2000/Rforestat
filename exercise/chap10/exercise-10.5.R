# 习题 10.5：Qn1 光合作用饱和曲线比较

library(minpack.lm)
library(ggplot2)
data("CO2", package = "datasets")
d <- subset(CO2, as.character(Plant) == "Qn1")
stopifnot(nrow(d) == 7L, all(d$conc > 0))

# （1）三个均值函数及起始值；前两个模型约束渐近值和速率参数为正。
mm_start <- list(Vmax = 45, Km = 100)
sat_start <- list(Asym = 40, k = 0.007)
log_start <- list(a = -20, b = 9)

mm_model <- nlsLM(uptake ~ Vmax * conc / (Km + conc), data = d, start = mm_start, lower = c(0, 0))
sat_model <- nlsLM(uptake ~ Asym * (1 - exp(-k * conc)), data = d, start = sat_start, lower = c(0, 0))
log_model <- nlsLM(uptake ~ a + b * log(conc), data = d, start = log_start)

# （2）报告收敛、参数、残差和可计算的 profile 区间。
models <- list(Michaelis_Menten = mm_model, exponential_saturation = sat_model, empirical_log = log_model)
for (name in names(models)) {
  cat("\n---", name, "---\n")
  print(summary(models[[name]]))
  interval <- tryCatch(confint(models[[name]]), error = function(e) conditionMessage(e))
  print(interval)
}

calc_r2 <- function(model) 1 - sum(residuals(model)^2) / sum((d$uptake - mean(d$uptake))^2)
comparison <- data.frame(
  model = names(models),
  AIC = vapply(models, AIC, numeric(1)),
  R2 = vapply(models, calc_r2, numeric(1))
)
print(comparison)

# （3）只在观测浓度范围展示拟合曲线并说明 n=7 限制。
grid <- data.frame(conc = seq(min(d$conc), max(d$conc), length.out = 200))
curves <- do.call(rbind, lapply(names(models), function(name) {
  data.frame(grid, uptake = predict(models[[name]], newdata = grid), model = name)
}))
if (!interactive()) grDevices::cairo_pdf(tempfile("exercise-10.5-", fileext = ".pdf"), width = 8, height = 5)
print(ggplot(d, aes(conc, uptake)) + geom_point() + geom_line(data = curves, aes(colour = model), linewidth = 1) + theme_minimal())
if (!interactive()) dev.off()
cat("Qn1 只有 7 个浓度点，AIC 和外推比较仅作函数拟合示范。\n")
