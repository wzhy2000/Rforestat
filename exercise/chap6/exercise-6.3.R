# 习题 6.3：向后逐步选择及按 Plant 分组验证

data("CO2", package = "datasets")

# （1）起始模型为三个主效应，最小模型为截距模型；记录 step() 路径。
full_model <- lm(uptake ~ conc + Treatment + Type, data = CO2)
selected_model <- step(
  full_model,
  scope = list(lower = ~1, upper = ~conc + Treatment + Type),
  direction = "backward",
  trace = 1
)
print(selected_model$anova)

# （2）列出保留项并在相同数据上比较 AIC/BIC。
cat("最终公式：", paste(deparse(formula(selected_model)), collapse = " "), "\n")
print(AIC(full_model, selected_model))
print(BIC(full_model, selected_model))

# （3）AIC 不能替代残差诊断。
if (!interactive()) grDevices::cairo_pdf(tempfile("exercise-6.3-", fileext = ".pdf"), width = 9, height = 6)
par(mfrow = c(1, 2))
plot(selected_model, which = 1)
plot(selected_model, which = 2)
if (!interactive()) dev.off()

# （4）留一 Plant 外层验证；每折只在训练数据中重新执行选择。
plants <- levels(CO2$Plant)
predictions <- lapply(plants, function(test_plant) {
  train <- CO2[CO2$Plant != test_plant, ]
  test <- CO2[CO2$Plant == test_plant, ]
  fold_full <- lm(uptake ~ conc + Treatment + Type, data = train)
  fold_selected <- step(
    fold_full,
    scope = list(lower = ~1, upper = ~conc + Treatment + Type),
    direction = "backward", trace = 0
  )
  data.frame(
    Plant = test_plant,
    observed = test$uptake,
    full = predict(fold_full, newdata = test),
    selected = predict(fold_selected, newdata = test)
  )
})
predictions <- do.call(rbind, predictions)

metrics <- function(observed, predicted) c(
  RMSE = sqrt(mean((observed - predicted)^2)),
  MAE = mean(abs(observed - predicted)),
  R2 = 1 - sum((observed - predicted)^2) / sum((observed - mean(observed))^2)
)
validation <- rbind(
  full = metrics(predictions$observed, predictions$full),
  selected = metrics(predictions$observed, predictions$selected)
)
print(validation)
cat("较低 AIC 只是在当前候选集内的相对证据，不保证模型假设或样本外预测更好。\n")
