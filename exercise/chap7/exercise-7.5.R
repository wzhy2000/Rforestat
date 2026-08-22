# 习题 7.5：Achimill 出现与管理方式的逻辑回归

library(vegan)
data("dune", package = "vegan")
data("dune.env", package = "vegan")

# （1）Achimill 是 Achillea millefolium 的盖度等级；只构造出现/未出现。
d <- transform(dune.env, Presence = as.integer(dune$Achimill > 0))
counts <- with(d, table(Management, Presence))
print(counts)
print(prop.table(counts, 1))
cat("20 个样地中的出现数：", sum(d$Presence), "\n")

# （2）拟合小样本二元逻辑回归并检查分离和估计稳定性。
model <- glm(Presence ~ Management, family = binomial, data = d)
print(summary(model))
if (any(!is.finite(coef(model))) || any(abs(coef(model)) > 10)) {
  cat("普通逻辑回归可能存在分离；正式分析应考虑 Firth 校正。\n")
}

# （3）报告各管理方式预测概率及区间，并明确优势比参照组。
newdata <- data.frame(Management = levels(d$Management))
link <- predict(model, newdata = newdata, type = "link", se.fit = TRUE)
probability_table <- transform(
  newdata,
  probability = plogis(link$fit),
  lower = plogis(link$fit - 1.96 * link$se.fit),
  upper = plogis(link$fit + 1.96 * link$se.fit)
)
print(probability_table)
odds_ratio <- cbind(OR = exp(coef(model)), exp(confint.default(model)))
print(odds_ratio)
cat("优势比参照水平：", levels(d$Management)[1], "\n")

# （4）预先声明阈值 0.5，实施留一法验证。
loo_probability <- vapply(seq_len(nrow(d)), function(i) {
  fold_model <- glm(Presence ~ Management, family = binomial, data = d[-i, ])
  predict(fold_model, newdata = d[i, , drop = FALSE], type = "response")
}, numeric(1))
loo_class <- as.integer(loo_probability >= 0.5)
print(table(observed = d$Presence, predicted = loo_class))
cat("dune 数值是盖度等级而非个体计数，不适用零膨胀 Poisson 或 hurdle 计数模型。\n")
