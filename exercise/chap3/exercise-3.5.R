# 习题 3.5：Loblolly 条件筛选和纵向轨迹

library(dplyr)
library(ggplot2)
data("Loblolly", package = "datasets")

# （1）核对纵向结构。Seed 标识树及其种源排序，不足以支持遗传家系推断。
structure_summary <- Loblolly |>
  summarise(n_tree = n_distinct(Seed), n = n())
per_tree <- Loblolly |> count(Seed, name = "n_observations")
print(structure_summary)
print(per_tree)
stopifnot(structure_summary$n_tree == 14L, all(per_tree$n_observations == 6L))

# （2）筛选并列出参与均值计算的观测。
selected <- Loblolly |>
  filter(age > 10, as.character(Seed) == "301")
print(selected)
mean_height <- mean(selected$height)
cat("Seed 301 在 age > 10 时的平均树高：", mean_height, "\n")

# （3）按树绘制树高-年龄轨迹；每树仅 6 点，不把平滑分布解释为稳定分布。
if (!interactive()) grDevices::cairo_pdf(tempfile("exercise-3.5-", fileext = ".pdf"), width = 9, height = 6)
print(
  ggplot(Loblolly, aes(x = age, y = height, group = Seed, colour = Seed)) +
    geom_line() +
    geom_point() +
    labs(title = "14 棵火炬松的树高-年龄轨迹", x = "年龄（年）", y = "树高（英尺）") +
    theme_minimal()
)
if (!interactive()) dev.off()
cat("Seed 不是可直接解释为 14 个遗传家系的谱系变量。\n")
