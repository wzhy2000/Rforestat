# 习题 3.3：按样地统计树木数量并评价样本量均衡性

library(dplyr)
library(ggplot2)
data("picea", package = "forestat")

# （1）按 PLOT1 统计并核对汇总总数。
counts <- picea |>
  count(PLOT1, name = "n") |>
  mutate(prop = n / sum(n))
print(counts)
stopifnot(sum(counts$n) == nrow(picea))

# （2）横向条形图适合精确比较；100% 构成图突出比例。
if (!interactive()) grDevices::cairo_pdf(tempfile("exercise-3.3-", fileext = ".pdf"), width = 9, height = 6)
print(
  ggplot(counts, aes(x = reorder(factor(PLOT1), n), y = n)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    labs(title = "各样地树木数量", x = "样地 PLOT1", y = "树木数量") +
    theme_minimal()
)
print(
  ggplot(counts, aes(x = "全部样地", y = prop, fill = factor(PLOT1))) +
    geom_col() +
    labs(title = "各样地样本构成", x = NULL, y = "比例", fill = "PLOT1") +
    theme_minimal()
)
if (!interactive()) dev.off()

# （3）用极差比和变异系数评价不均衡。
balance <- counts |>
  summarise(
    min_n = min(n),
    max_n = max(n),
    max_min_ratio = max(n) / min(n),
    cv = sd(n) / mean(n)
  )
print(balance)
cat("若样地不均衡，模型验证应按 PLOT1 分组；必要时采用加权、分层抽样并报告各组不确定性。\n")
