# 习题 13.3：螨类 Shannon 多样性的贝叶斯线性与二次模型

library(vegan)
library(brms)
library(loo)
library(posterior)

options(mc.cores = min(2L, parallel::detectCores(logical = FALSE)))
data("mite", package = "vegan")
data("mite.env", package = "vegan")
d <- transform(mite.env, Shannon = diversity(mite, index = "shannon"))
d <- d[complete.cases(d[c("Shannon", "SubsDens", "WatrCont")]), ]
s_center <- mean(d$SubsDens); s_scale <- sd(d$SubsDens)
w_center <- mean(d$WatrCont); w_scale <- sd(d$WatrCont)
d$zS <- (d$SubsDens - s_center) / s_scale
d$zW <- (d$WatrCont - w_center) / w_scale
cat("样方数：", nrow(d), "\n")
print(summary(d[c("Shannon", "SubsDens", "WatrCont")]))
print(data.frame(variable = c("SubsDens", "WatrCont"), center = c(s_center, w_center), scale = c(s_scale, w_scale)))

priors <- c(
  prior(normal(0, 0.5), class = "b"),
  prior(normal(1.5, 1), class = "Intercept"),
  prior(exponential(2), class = "sigma")
)
set.seed(123)
prior_index <- sample(seq_len(nrow(d)), 1000L, replace = TRUE)
prior_prediction <- rnorm(
  1000L,
  rnorm(1000L, 1.5, 1) + rnorm(1000L, 0, 0.5) * d$zS[prior_index] + rnorm(1000L, 0, 0.5) * d$zW[prior_index],
  rexp(1000L, 2)
)
cat("线性模型先验预测的 1%、50%、99% 分位数：\n"); print(quantile(prior_prediction, c(0.01, 0.5, 0.99)))
model_linear <- brm(
  Shannon ~ zS + zW, data = d, prior = priors,
  chains = 2, iter = 1000, warmup = 500, seed = 123, refresh = 0
)
model_quadratic <- brm(
  Shannon ~ zS + I(zS^2) + zW + I(zW^2), data = d, prior = priors,
  chains = 2, iter = 1000, warmup = 500, seed = 124, refresh = 0
)
print(summary(model_linear)); print(summary(model_quadratic))
divergences <- function(model) {
  params <- rstan::get_sampler_params(model$fit, inc_warmup = FALSE)
  sum(vapply(params, function(x) sum(x[, "divergent__"]), numeric(1)))
}
cat("发散数：线性", divergences(model_linear), "；二次", divergences(model_quadratic), "\n")
loo_linear <- loo(model_linear)
loo_quadratic <- loo(model_quadratic)
print(loo_compare(list(linear = loo_linear, quadratic = loo_quadratic)))
cat("最大 Pareto-k：线性", max(loo_linear$diagnostics$pareto_k), "；二次", max(loo_quadratic$diagnostics$pareto_k), "\n")

draws <- as_draws_df(model_quadratic)
needed_draws <- c("b_zS", "b_IzSE2", "b_zW", "b_IzWE2")
stopifnot(all(needed_draws %in% names(draws)))
derivative_summary <- function(grid, linear_name, quadratic_name, center, scale, label) {
  z <- (grid - center) / scale
  derivative <- outer(draws[[quadratic_name]], 2 * z, `*`)
  derivative <- sweep(derivative, 1, draws[[linear_name]], `+`) / scale
  interval <- t(apply(derivative, 2, quantile, probs = c(0.025, 0.5, 0.975)))
  data.frame(variable = label, value = grid, lower = interval[, 1], median = interval[, 2], upper = interval[, 3], clear_direction = interval[, 1] > 0 | interval[, 3] < 0)
}
s_grid <- seq(min(d$SubsDens), max(d$SubsDens), length.out = 100)
w_grid <- seq(min(d$WatrCont), max(d$WatrCont), length.out = 100)
derivatives <- rbind(
  derivative_summary(s_grid, "b_zS", "b_IzSE2", s_center, s_scale, "SubsDens"),
  derivative_summary(w_grid, "b_zW", "b_IzWE2", w_center, w_scale, "WatrCont")
)
for (name in unique(derivatives$variable)) {
  clear <- subset(derivatives, variable == name & clear_direction)
  if (nrow(clear) == 0L) {
    cat(name, "：观测范围内没有导数 95% 区间排除 0 的网格点。\n")
  } else {
    cat(name, "：方向较明确的网格范围", min(clear$value), "至", max(clear$value), "（可能不连续，详见表）。\n")
  }
}

if (!interactive()) grDevices::cairo_pdf(tempfile("exercise-13.3-", fileext = ".pdf"), width = 9, height = 6)
print(pp_check(model_quadratic, ndraws = 50))
plot(derivatives$value, derivatives$median, type = "n", xlab = "环境变量原尺度", ylab = "响应曲线导数")
for (name in unique(derivatives$variable)) {
  z <- subset(derivatives, variable == name)
  lines(z$value, z$median, col = if (name == "SubsDens") "steelblue" else "darkorange", lwd = 2)
}
abline(h = 0, lty = 2)
legend("topright", unique(derivatives$variable), col = c("steelblue", "darkorange"), lty = 1, lwd = 2)
if (!interactive()) dev.off()
