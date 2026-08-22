# 习题 7.4：高光合作用速率的逻辑回归与按 Plant 验证

data("CO2", package = "datasets")

# （1）30 是教学用操作阈值，不表示统计“显著”。
d <- transform(CO2, high_uptake = as.integer(uptake > 30))
print(table(d$high_uptake))

# （2）拟合简单逻辑回归，报告每 100 浓度单位的优势比及区间。
model <- glm(high_uptake ~ conc, family = binomial, data = d)
print(summary(model))
coefficient_interval <- confint.default(model)["conc", ]
or_100 <- exp(100 * c(estimate = coef(model)["conc"], lower = coefficient_interval[1], upper = coefficient_interval[2]))
print(or_100)

# （3）在 link 尺度构造区间，再转回响应概率。
grid <- data.frame(conc = seq(min(d$conc), max(d$conc), length.out = 200))
link_prediction <- predict(model, newdata = grid, type = "link", se.fit = TRUE)
grid$fit <- plogis(link_prediction$fit)
grid$lower <- plogis(link_prediction$fit - 1.96 * link_prediction$se.fit)
grid$upper <- plogis(link_prediction$fit + 1.96 * link_prediction$se.fit)

if (!interactive()) grDevices::cairo_pdf(tempfile("exercise-7.4-", fileext = ".pdf"), width = 7, height = 5)
plot(d$conc, d$high_uptake, pch = 16, col = rgb(0, 0, 0, 0.35), xlab = "CO2 浓度", ylab = "高 uptake 概率")
polygon(c(grid$conc, rev(grid$conc)), c(grid$lower, rev(grid$upper)), border = NA, col = rgb(0.2, 0.4, 0.8, 0.2))
lines(grid$conc, grid$fit, col = "steelblue", lwd = 2)
if (!interactive()) dev.off()

# （4）留一 Plant 分组验证；预先声明分类阈值为 0.5。
plants <- levels(d$Plant)
validation <- do.call(rbind, lapply(plants, function(test_plant) {
  train <- d[d$Plant != test_plant, ]
  test <- d[d$Plant == test_plant, ]
  fold_model <- glm(high_uptake ~ conc, family = binomial, data = train)
  data.frame(observed = test$high_uptake, probability = predict(fold_model, newdata = test, type = "response"))
}))

auc_rank <- function(observed, probability) {
  n_positive <- sum(observed == 1)
  n_negative <- sum(observed == 0)
  (sum(rank(probability)[observed == 1]) - n_positive * (n_positive + 1) / 2) /
    (n_positive * n_negative)
}
classification <- as.integer(validation$probability >= 0.5)
cat("留一 Plant ROC-AUC：", auc_rank(validation$observed, validation$probability), "\n")
print(table(observed = validation$observed, predicted = classification))
cat("简单 GLM 忽略株内相关；正式推断应使用混合逻辑模型。\n")
