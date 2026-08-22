# 习题 10.1：Shannon 多样性的幂函数与指数函数模型

library(vegan)
library(minpack.lm)
library(ggplot2)
data("mite", package = "vegan")
data("mite.env", package = "vegan")

d <- transform(mite.env, Shannon = diversity(mite, index = "shannon"))
d <- d[complete.cases(d[c("Shannon", "WatrCont")]), ]

# （1）幂函数要求 WatrCont 严格为正。
stopifnot(all(d$WatrCont > 0))
print(summary(d[c("Shannon", "WatrCont")]))

calc_r2 <- function(model, observed) {
  predicted <- predict(model)
  1 - sum((observed - predicted)^2) / sum((observed - mean(observed))^2)
}

# （2）幂函数；记录起始值并使用正尺度参数约束。
power_start <- list(a = 3, b = 0)
power_model <- nlsLM(
  Shannon ~ a * WatrCont^b,
  data = d, start = power_start,
  lower = c(a = 0, b = -Inf)
)
print(summary(power_model))

# （3）指数函数。
exponential_start <- list(a = 2, b = 0)
exponential_model <- nlsLM(
  Shannon ~ a * exp(b * WatrCont),
  data = d, start = exponential_start,
  lower = c(a = 0, b = -Inf)
)
print(summary(exponential_model))

comparison <- data.frame(
  model = c("power", "exponential"),
  AIC = c(AIC(power_model), AIC(exponential_model)),
  R2 = c(calc_r2(power_model, d$Shannon), calc_r2(exponential_model, d$Shannon))
)
print(comparison)

# （4）相同五折和指标比较验证误差。
set.seed(123)
fold_id <- sample(rep(1:5, length.out = nrow(d)))
cv <- do.call(rbind, lapply(1:5, function(fold) {
  train <- d[fold_id != fold, ]
  test <- d[fold_id == fold, ]
  mp <- nlsLM(Shannon ~ a * WatrCont^b, data = train, start = power_start, lower = c(0, -Inf))
  me <- nlsLM(Shannon ~ a * exp(b * WatrCont), data = train, start = exponential_start, lower = c(0, -Inf))
  data.frame(
    observed = test$Shannon,
    power = predict(mp, newdata = test),
    exponential = predict(me, newdata = test)
  )
}))
metrics <- function(predicted) c(
  RMSE = sqrt(mean((cv$observed - predicted)^2)),
  MAE = mean(abs(cv$observed - predicted))
)
print(rbind(power = metrics(cv$power), exponential = metrics(cv$exponential)))

grid <- data.frame(WatrCont = seq(min(d$WatrCont), max(d$WatrCont), length.out = 200))
curves <- rbind(
  data.frame(grid, Shannon = predict(power_model, newdata = grid), model = "幂函数"),
  data.frame(grid, Shannon = predict(exponential_model, newdata = grid), model = "指数函数")
)
if (!interactive()) grDevices::cairo_pdf(tempfile("exercise-10.1-", fileext = ".pdf"), width = 9, height = 5)
print(
  ggplot(d, aes(WatrCont, Shannon)) +
    geom_point() + geom_line(data = curves, aes(colour = model), linewidth = 1) +
    labs(title = "土壤含水量与 Shannon 多样性", x = "WatrCont", y = "Shannon") +
    theme_minimal()
)
if (!interactive()) dev.off()
cat("模型优劣只限于当前加性误差假设和观测范围；负指数参数的长期外推不能自动赋予生态意义。\n")
