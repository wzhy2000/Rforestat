# 习题 12.1：云杉胸径—地上生物量递归非线性方程系统

library(forestat)
library(minpack.lm)
library(systemfit)

data("picea", package = "forestat")
picea$AGB <- with(picea, STEM + BRANCH + FOLIAGE + FRUIT)
needed <- c("D0", "LH", "CPA", "AGB")
d <- subset(picea, complete.cases(picea[needed]) & D0 > 0 & LH > 0 & CPA >= 0 & AGB > 0)
cat("分析样本量：", nrow(d), "\n")
print(summary(d[needed]))

# （1）教材指定的递归均值函数；第二式使用观测 D0。
eq_diameter <- D0 ~ b1 * exp(b2 * LH - b3 * CPA)
eq_biomass <- AGB ~ a1 * D0^a2 * LH^a3

# （2）由两个对数线性辅助模型产生数据驱动起始值。
start_d_fit <- lm(log(D0) ~ LH + CPA, data = d)
start_b_fit <- lm(log(AGB) ~ log(D0) + log(LH), data = d)
start_d <- c(
  b1 = exp(unname(coef(start_d_fit)[1])),
  b2 = unname(coef(start_d_fit)[2]),
  b3 = -unname(coef(start_d_fit)[3])
)
start_b <- c(
  a1 = exp(unname(coef(start_b_fit)[1])),
  a2 = unname(coef(start_b_fit)[2]),
  a3 = unname(coef(start_b_fit)[3])
)
cat("逐方程起始值：\n"); print(c(start_d, start_b))

fit_d <- nlsLM(eq_diameter, data = d, start = as.list(start_d), control = nls.lm.control(maxiter = 300))
fit_b <- nlsLM(eq_biomass, data = d, start = as.list(start_b), control = nls.lm.control(maxiter = 300))

equation_metrics <- function(model, observed) {
  residual <- observed - fitted(model)
  c(
    RMSE = sqrt(mean(residual^2)),
    R2 = 1 - sum(residual^2) / sum((observed - mean(observed))^2)
  )
}
cat("逐方程 D0 参数与标准误：\n"); print(coef(summary(fit_d))[, 1:2, drop = FALSE])
cat("逐方程 AGB 参数与标准误：\n"); print(coef(summary(fit_b))[, 1:2, drop = FALSE])
print(rbind(D0 = equation_metrics(fit_d, d$D0), AGB = equation_metrics(fit_b, d$AGB)))

# （3）在同一观测、同一均值函数和递归外生假设下比较非线性 OLS 与 SUR。
system_equations <- list(D0 = eq_diameter, AGB = eq_biomass)
system_start <- c(coef(fit_d), coef(fit_b))
fit_ols <- nlsystemfit("OLS", system_equations, startvals = system_start, data = d)
fit_sur <- nlsystemfit("SUR", system_equations, startvals = system_start, data = d)

for (i in seq_along(system_equations)) {
  cat("\n方程", names(system_equations)[i], "\n")
  print(rbind(
    OLS = c(fit_ols$eq[[i]]$b, RMSE = fit_ols$eq[[i]]$rmse, R2 = fit_ols$eq[[i]]$r2),
    SUR = c(fit_sur$eq[[i]]$b, RMSE = fit_sur$eq[[i]]$rmse, R2 = fit_sur$eq[[i]]$r2)
  ))
  cat("SUR 标准误：\n"); print(fit_sur$eq[[i]]$se)
}

# （4）残差相关仅说明联合估计可能有效率收益，不等于解决内生性。
residual_correlation <- cor(fit_sur$eq[[1]]$residuals, fit_sur$eq[[2]]$residuals)
cat("SUR 两方程残差相关：", residual_correlation, "\n")
cat("若观测 D0 与第二式扰动相关，SUR 仍有内生性偏误，需另找满足排除限制的有效工具变量。\n")
