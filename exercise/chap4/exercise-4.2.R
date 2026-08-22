# 习题 4.2：两个预先确定样地的 Welch t 检验

library(dplyr)
library(ggplot2)
data("picea", package = "forestat")

# （1）按可复现规则确定两个样地，并检查样本量、缺失值和极端观测。
plot_ids <- sort(unique(picea$PLOT1))[1:2]
d <- picea |>
  filter(PLOT1 %in% plot_ids, !is.na(D0)) |>
  mutate(PLOT1 = factor(PLOT1, levels = plot_ids))
cat("比较的 PLOT1：", paste(plot_ids, collapse = " 与 "), "\n")
group_summary <- d |>
  group_by(PLOT1) |>
  summarise(n = n(), mean = mean(D0), sd = sd(D0), min = min(D0), max = max(D0), .groups = "drop")
print(group_summary)

# （2）比较分布形状、离群值和方差差异。
if (!interactive()) grDevices::cairo_pdf(tempfile("exercise-4.2-", fileext = ".pdf"), width = 7, height = 5)
print(
  ggplot(d, aes(x = PLOT1, y = D0, fill = PLOT1)) +
    geom_boxplot(outlier.shape = NA, alpha = 0.5) +
    geom_jitter(width = 0.08, alpha = 0.6) +
    labs(title = "两个样地的实测胸径分布", x = "样地 PLOT1", y = "实测胸径 D0") +
    theme_minimal()
)
if (!interactive()) dev.off()

# （3）两组方差和样本量可不同，使用双侧 Welch t 检验。
welch_test <- t.test(D0 ~ PLOT1, data = d, var.equal = FALSE, alternative = "two.sided")
print(welch_test)
mean_difference <- group_summary$mean[1] - group_summary$mean[2]
cat("均值差（", plot_ids[1], " - ", plot_ids[2], "）：", mean_difference, "\n", sep = "")
cat("95% 置信区间：", paste(welch_test$conf.int, collapse = ", "), "\n")

# （4）不同树观测的独立性来自采样设计，而不是图形；结论只适用于所选两个样地。
cat("推断范围仅限上述两个样地，不能自动推广到其他样地或作因果解释。\n")
