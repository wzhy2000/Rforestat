# 习题 12.2：强、弱工具变量下 2SLS 与 3SLS 的 Monte Carlo 比较

library(systemfit)

set.seed(123)
B <- 300L
n <- 300L
true_beta <- 1.5

# z1、z2 只通过内生变量 x 影响 y；u 与 x 的扰动 v 相关，形成内生性。
one_run <- function(pi_strength) {
  z1 <- rnorm(n)
  z2 <- rnorm(n)
  w <- rnorm(n)
  u <- rnorm(n)
  independent_noise <- rnorm(n)
  v <- 0.6 * u + sqrt(1 - 0.6^2) * independent_noise
  x <- pi_strength * z1 + pi_strength * z2 + 0.5 * w + v
  y <- 1 + true_beta * x + 0.5 * w + u
  d <- data.frame(x, y, z1, z2, w)

  equations <- list(x = x ~ z1 + z2 + w, y = y ~ x + w)
  instruments <- ~z1 + z2 + w
  first_stage_full <- lm(x ~ z1 + z2 + w, data = d)
  first_stage_restricted <- lm(x ~ w, data = d)
  partial_f <- anova(first_stage_restricted, first_stage_full)$F[2]

  extract_result <- function(method) {
    fit <- systemfit(equations, method = method, inst = instruments, data = d)
    estimate <- unname(coef(fit)["y_x"])
    standard_error <- unname(sqrt(diag(vcov(fit)))["y_x"])
    c(estimate = estimate, se = standard_error)
  }
  c(F = partial_f, SLS2 = extract_result("2SLS"), SLS3 = extract_result("3SLS"))
}

summarize_simulation <- function(pi_strength, label) {
  simulations <- t(replicate(B, one_run(pi_strength)))
  summarize_method <- function(prefix) {
    estimate <- simulations[, paste0(prefix, ".estimate")]
    standard_error <- simulations[, paste0(prefix, ".se")]
    c(
      Bias = mean(estimate - true_beta),
      EmpiricalSD = sd(estimate),
      MeanSE = mean(standard_error),
      RMSE = sqrt(mean((estimate - true_beta)^2)),
      Coverage95 = mean(abs(estimate - true_beta) <= 1.96 * standard_error),
      MeanFirstStageF = mean(simulations[, "F"])
    )
  }
  rbind(
    data.frame(Strength = label, Method = "2SLS", t(summarize_method("SLS2"))),
    data.frame(Strength = label, Method = "3SLS", t(summarize_method("SLS3")))
  )
}

results <- rbind(
  summarize_simulation(pi_strength = 1, label = "强工具变量 (pi=1)"),
  summarize_simulation(pi_strength = 0.1, label = "弱工具变量 (pi=0.1)")
)
print(results, row.names = FALSE)
cat("每种强度完成", B, "次重复；β 真值为", true_beta, "。\n")
cat("弱工具变量下的偏差、RMSE 与覆盖率变化由模拟汇总判断，不能由单次样本或无条件定理替代。\n")
