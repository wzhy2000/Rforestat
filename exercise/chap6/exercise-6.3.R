source("exercise-6.2.R")
# 向后逐步回归（默认使用 AIC 作为选择标准）
step_model <- step(model, direction = "backward", trace = FALSE)
summary(step_model)
AIC(step_model)
# 模型诊断图（逐步回归模型）
par(mfrow = c(1, 2))
plot(step_model, which = 1)
plot(step_model, which = 2)
