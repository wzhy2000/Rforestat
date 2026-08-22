# 习题 13.4：云杉地上生物量回归的先验敏感性

library(forestat)
library(brms)
library(loo)
library(posterior)

options(mc.cores = min(2L, parallel::detectCores(logical = FALSE)))
data("picea", package = "forestat")
picea$AGB <- with(picea, STEM + BRANCH + FOLIAGE + FRUIT)
d <- subset(picea, complete.cases(AGB, D0, LH, CPA) & AGB > 0)
d$log_AGB <- log(d$AGB)
transformations <- lapply(d[c("D0", "LH", "CPA")], function(x) c(center = mean(x), scale = sd(x)))
d$zD <- as.numeric(scale(d$D0)); d$zH <- as.numeric(scale(d$LH)); d$zC <- as.numeric(scale(d$CPA))
cat("样本量：", nrow(d), "；AGB=STEM+BRANCH+FOLIAGE+FRUIT。\n")
print(do.call(rbind, transformations))

weak_prior <- c(
  prior(normal(0, 1), class = "b"),
  prior(normal(3, 2), class = "Intercept"),
  prior(exponential(1), class = "sigma")
)
# 这是显式的方向性敏感性情景，不冒充已完成换算的文献信息先验。
# 出版时若要称“文献信息先验”，必须用给定树种、单位及标准化尺度重新换算并补充出处。
directional_prior <- c(
  prior(normal(0.8, 0.4), class = "b", coef = "zD"),
  prior(normal(0.3, 0.4), class = "b", coef = "zH"),
  prior(normal(0.2, 0.4), class = "b", coef = "zC"),
  prior(normal(3, 2), class = "Intercept"),
  prior(exponential(1), class = "sigma")
)
set.seed(123)
prior_index <- sample(seq_len(nrow(d)), 1000L, replace = TRUE)
simulate_prior_prediction <- function(directional = FALSE) {
  beta_d <- if (directional) rnorm(1000L, 0.8, 0.4) else rnorm(1000L, 0, 1)
  beta_h <- if (directional) rnorm(1000L, 0.3, 0.4) else rnorm(1000L, 0, 1)
  beta_c <- if (directional) rnorm(1000L, 0.2, 0.4) else rnorm(1000L, 0, 1)
  mu <- rnorm(1000L, 3, 2) + beta_d * d$zD[prior_index] + beta_h * d$zH[prior_index] + beta_c * d$zC[prior_index]
  rnorm(1000L, mu, rexp(1000L, 1))
}
cat("弱信息先验预测分位数：\n"); print(quantile(simulate_prior_prediction(FALSE), c(0.01, 0.5, 0.99)))
cat("方向性先验预测分位数：\n"); print(quantile(simulate_prior_prediction(TRUE), c(0.01, 0.5, 0.99)))

model_weak <- brm(
  log_AGB ~ zD + zH + zC, data = d, prior = weak_prior,
  chains = 2, iter = 1000, warmup = 500, seed = 123, refresh = 0
)
model_directional <- brm(
  log_AGB ~ zD + zH + zC, data = d, prior = directional_prior,
  chains = 2, iter = 1000, warmup = 500, seed = 124, refresh = 0
)
print(summary(model_weak)); print(summary(model_directional))
divergences <- function(model) {
  params <- rstan::get_sampler_params(model$fit, inc_warmup = FALSE)
  sum(vapply(params, function(x) sum(x[, "divergent__"]), numeric(1)))
}
cat("发散数：弱信息", divergences(model_weak), "；方向性", divergences(model_directional), "\n")
loo_weak <- loo(model_weak)
loo_directional <- loo(model_directional)
print(loo_compare(list(weak = loo_weak, directional = loo_directional)))
cat("最大 Pareto-k：弱信息", max(loo_weak$diagnostics$pareto_k), "；方向性", max(loo_directional$diagnostics$pareto_k), "\n")
cat("若 Pareto-k > 0.7，应报告影响点并考虑 moment matching 或重新拟合留一法。\n")

posterior_table <- function(model, label) {
  draws <- as_draws_df(model)
  parameters <- c("b_Intercept", "b_zD", "b_zH", "b_zC", "sigma")
  do.call(rbind, lapply(parameters, function(parameter) {
    x <- draws[[parameter]]
    data.frame(model = label, parameter, mean = mean(x), lower = quantile(x, 0.025), upper = quantile(x, 0.975))
  }))
}
print(rbind(posterior_table(model_weak, "weak"), posterior_table(model_directional, "directional")), row.names = FALSE)

new_tree <- data.frame(zD = 0, zH = 0, zC = 0)
prediction_intervals <- rbind(
  weak = quantile(posterior_predict(model_weak, newdata = new_tree), c(0.025, 0.5, 0.975)),
  directional = quantile(posterior_predict(model_directional, newdata = new_tree), c(0.025, 0.5, 0.975))
)
print(prediction_intervals)
cat("directional 情景仅用于先验敏感性；没有可核实出处和尺度换算时，不能标成文献信息先验。\n")

if (!interactive()) grDevices::cairo_pdf(tempfile("exercise-13.4-", fileext = ".pdf"), width = 9, height = 5)
print(pp_check(model_weak, ndraws = 50))
print(pp_check(model_directional, ndraws = 50))
if (!interactive()) dev.off()
