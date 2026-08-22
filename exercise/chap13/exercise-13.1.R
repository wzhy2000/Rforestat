# 习题 13.1：土壤 CO2 通量的贝叶斯线性回归

library(fortedata)
library(brms)
library(posterior)

options(mc.cores = min(2L, parallel::detectCores(logical = FALSE)))
raw_data <- fd_soil_respiration()
needed <- c("soil_co2_efflux", "soil_temp", "vwc")
d <- raw_data[complete.cases(raw_data[needed]) & raw_data$soil_co2_efflux > 0, needed]
d$log_efflux <- log(d$soil_co2_efflux)
temp_center <- mean(d$soil_temp); temp_scale <- sd(d$soil_temp)
vwc_center <- mean(d$vwc); vwc_scale <- sd(d$vwc)
d$ztemp <- (d$soil_temp - temp_center) / temp_scale
d$zvwc <- (d$vwc - vwc_center) / vwc_scale
cat("分析样本量：", nrow(d), "；排除非正或缺失：", nrow(raw_data) - nrow(d), "\n")
print(data.frame(variable = c("soil_temp", "vwc"), center = c(temp_center, vwc_center), scale = c(temp_scale, vwc_scale)))

priors <- c(
  prior(normal(0, 1), class = "b"),
  prior(normal(0, 2), class = "Intercept"),
  prior(exponential(1), class = "sigma")
)

# （2）先验预测：在观测协变量上直接从同一先验生成响应，检查 log 通量尺度。
set.seed(123)
prior_rows <- sample(seq_len(nrow(d)), min(500L, nrow(d)))
prior_draws <- 500L
prior_intercept <- rnorm(prior_draws, 0, 2)
prior_b_temp <- rnorm(prior_draws, 0, 1)
prior_b_vwc <- rnorm(prior_draws, 0, 1)
prior_sigma <- rexp(prior_draws, 1)
prior_y <- vapply(seq_len(prior_draws), function(i) {
  mu <- prior_intercept[i] + prior_b_temp[i] * d$ztemp[prior_rows] + prior_b_vwc[i] * d$zvwc[prior_rows]
  sample(rnorm(length(mu), mu, prior_sigma[i]), 1L)
}, numeric(1))
cat("先验预测 log 通量的 1%、50%、99% 分位数：\n"); print(quantile(prior_y, c(0.01, 0.5, 0.99)))

model <- brm(
  log_efflux ~ ztemp + zvwc,
  data = d, prior = priors,
  chains = 2, iter = 1000, warmup = 500,
  seed = 123, refresh = 0, sample_prior = "yes"
)
print(summary(model))
diagnostics <- summarise_draws(as_draws_array(model), "mean", "sd", "rhat", "ess_bulk", "ess_tail")
print(subset(diagnostics, variable %in% c("b_Intercept", "b_ztemp", "b_zvwc", "sigma")))
sampler <- rstan::get_sampler_params(model$fit, inc_warmup = FALSE)
cat("发散数：", sum(vapply(sampler, function(x) sum(x[, "divergent__"]), numeric(1))), "\n")

if (!interactive()) grDevices::cairo_pdf(tempfile("exercise-13.1-", fileext = ".pdf"), width = 9, height = 6)
print(pp_check(model, ndraws = 50))
epred <- colMeans(posterior_epred(model, ndraws = 200))
plot(d$log_efflux, epred, pch = 16, cex = 0.45, xlab = "观测 log 通量", ylab = "后验预测均值")
abline(0, 1, lty = 2, col = "red")
if (!interactive()) dev.off()
cat("exp(系数) 表示对应标准化预测变量增加 1 SD 时几何均值的条件乘法关联，不是因果效应。\n")
