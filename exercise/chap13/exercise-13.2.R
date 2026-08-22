# 习题 13.2：Loblolly 树高的贝叶斯随机截距模型

library(brms)
library(loo)
library(posterior)

options(mc.cores = min(2L, parallel::detectCores(logical = FALSE)))
data("Loblolly", package = "datasets")
d <- Loblolly

priors <- c(
  prior(normal(0, 5), class = "b"),
  prior(student_t(3, 0, 10), class = "Intercept"),
  prior(exponential(1), class = "sd"),
  prior(exponential(1), class = "sigma")
)
model <- brm(
  height ~ age + (1 | Seed), data = d, prior = priors,
  chains = 4, iter = 1000, warmup = 500,
  seed = 123, refresh = 0, sample_prior = "yes"
)
print(summary(model))
diagnostics <- summarise_draws(as_draws_array(model), "rhat", "ess_bulk", "ess_tail")
print(subset(diagnostics, grepl("^b_|^sd_|^sigma$", variable)))
sampler <- rstan::get_sampler_params(model$fit, inc_warmup = FALSE)
cat("发散数：", sum(vapply(sampler, function(x) sum(x[, "divergent__"]), numeric(1))), "\n")

seed_array <- ranef(model, summary = TRUE)$Seed
if ("Intercept" %in% dimnames(seed_array)[[2]]) {
  seed_effect <- seed_array[, "Intercept", ]
} else {
  seed_effect <- seed_array[, , "Intercept"]
}
seed_table <- data.frame(Seed = rownames(seed_effect), seed_effect, row.names = NULL)
print(seed_table[order(-seed_table$Estimate), ])
cat("随机截距后验向总体收缩；排序仅描述树木差异，不代表家系遗传差异。\n")

print(bayes_R2(model))
loo_result <- loo(model)
print(loo_result)
cat("单模型 LOO 是预测适配摘要，不能单独证明该模型优于未拟合的候选模型。\n")

if (!interactive()) grDevices::cairo_pdf(tempfile("exercise-13.2-", fileext = ".pdf"), width = 8, height = 5)
print(pp_check(model, ndraws = 50))
if (!interactive()) dev.off()
