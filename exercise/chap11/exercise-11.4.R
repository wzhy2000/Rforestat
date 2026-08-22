# 习题 11.4：dune 群落总盖度的非线性模型

library(vegan)
library(minpack.lm)
library(nlme)

data("dune", package = "vegan")
data("dune.env", package = "vegan")

d <- dune.env
d$TotalCover <- rowSums(dune)
d$MoistureScore <- as.numeric(d$Moisture)
d$Management <- factor(d$Management)

stopifnot(nrow(d) == nrow(dune), is.ordered(d$Moisture))
cat("样方数：", nrow(d), "；物种数：", ncol(dune), "\n")
cat("Moisture 的有序水平及其数值编码：\n")
print(data.frame(
  Moisture = levels(d$Moisture),
  MoistureScore = seq_along(levels(d$Moisture))
), row.names = FALSE)
cat("各水分水平的样本量和总盖度均值：\n")
print(aggregate(TotalCover ~ Moisture, d, function(x) c(n = length(x), mean = mean(x))))

# （1）用单峰曲线描述总盖度随有序水分梯度的变化。
# 样本仅有 20 个，故将曲线宽度固定为一个水分等级，避免过度参数化。
moisture_formula <- TotalCover ~
  base + amplitude * exp(-0.5 * (MoistureScore - optimum)^2)

initial_fit <- nlsLM(
  moisture_formula,
  data = d,
  start = c(base = 30, amplitude = 8, optimum = 2.5),
  lower = c(base = 0, amplitude = -Inf, optimum = 1),
  upper = c(base = Inf, amplitude = Inf, optimum = 4)
)

equal_variance_model <- gnls(
  moisture_formula,
  data = d,
  start = coef(initial_fit)
)

moisture_grid <- data.frame(
  Moisture = ordered(levels(d$Moisture), levels = levels(d$Moisture)),
  MoistureScore = seq_along(levels(d$Moisture))
)
fixed_coef <- coef(equal_variance_model)
moisture_grid$predicted_TotalCover <- with(
  moisture_grid,
  fixed_coef["base"] + fixed_coef["amplitude"] *
    exp(-0.5 * (MoistureScore - fixed_coef["optimum"])^2)
)
cat("水分梯度基础模型的固定效应参数：\n")
print(fixed_coef)
cat("各 Moisture 水平的固定效应曲线预测：\n")
print(moisture_grid, row.names = FALSE)

# （2）把 Management 作为固定效应加入基线参数。
# BF 是参照水平，其余三个管理水平用哑变量表示。
d$M_HF <- as.numeric(d$Management == "HF")
d$M_NM <- as.numeric(d$Management == "NM")
d$M_SF <- as.numeric(d$Management == "SF")
management_formula <- TotalCover ~
  base + M_HF * delta_HF + M_NM * delta_NM + M_SF * delta_SF +
  amplitude * exp(-0.5 * (MoistureScore - optimum)^2)

management_start <- c(
  base = unname(fixed_coef["base"]),
  delta_HF = 0,
  delta_NM = 0,
  delta_SF = 0,
  amplitude = unname(fixed_coef["amplitude"]),
  optimum = unname(fixed_coef["optimum"])
)
management_model <- gnls(
  management_formula,
  data = d,
  start = management_start
)
cat("加入 Management 固定效应后的参数：\n")
print(coef(management_model))

# （3）Management 只有 4 个水平，不宜依靠这 4 组稳定估计随机效应方差。
cat(
  "若有更多管理水平，可用 nlme() 将某个非线性参数写成 random = parameter ~ 1 | Management；",
  "本数据仅有 4 个水平，因此这里只说明方法，不报告不稳定的随机效应模型。\n",
  sep = ""
)

# （4）比较同方差模型与按 Management 分组的 varIdent 方差结构。
heterogeneous_variance_model <- update(
  management_model,
  weights = varIdent(form = ~1 | Management)
)
comparison <- data.frame(
  model = c("Management 固定效应、同方差", "Management 固定效应、varIdent"),
  AIC = c(AIC(management_model), AIC(heterogeneous_variance_model)),
  residual_SD = c(sigma(management_model), sigma(heterogeneous_variance_model))
)
print(comparison, row.names = FALSE)
cat("varIdent 相对于参照组的标准差比：\n")
print(coef(heterogeneous_variance_model$modelStruct$varStruct, unconstrained = FALSE))

if (!interactive()) {
  grDevices::cairo_pdf(tempfile("exercise-11.4-", fileext = ".pdf"), width = 9, height = 5)
}
old_par <- par(mfrow = c(1, 2))
plot(
  fitted(management_model), resid(management_model, type = "pearson"),
  xlab = "同方差模型拟合值", ylab = "Pearson 残差",
  main = "同方差结构"
)
abline(h = 0, lty = 2, col = "grey40")
plot(
  fitted(heterogeneous_variance_model),
  resid(heterogeneous_variance_model, type = "pearson"),
  xlab = "varIdent 模型拟合值", ylab = "Pearson 残差",
  main = "Management 异方差结构"
)
abline(h = 0, lty = 2, col = "grey40")
par(old_par)
if (!interactive()) dev.off()

cat("应结合 AIC 与残差图判断 varIdent 是否改善拟合，不能只因管理组不同就默认异方差。\n")
