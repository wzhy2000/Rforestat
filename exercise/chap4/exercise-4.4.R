# 习题 4.4：两棵树同龄观测的配对分析

library(dplyr)
library(tidyr)
library(ggplot2)
data("Loblolly", package = "datasets")

# （1）绘制轨迹并按相同年龄对齐观测。
d <- Loblolly |>
  filter(as.character(Seed) %in% c("301", "303")) |>
  mutate(Seed = as.character(Seed))
wide <- d |>
  select(age, Seed, height) |>
  pivot_wider(names_from = Seed, values_from = height) |>
  arrange(age) |>
  mutate(diff = `301` - `303`)
print(wide)
stopifnot(!anyNA(wide[c("301", "303")]))

if (!interactive()) grDevices::cairo_pdf(tempfile("exercise-4.4-", fileext = ".pdf"), width = 9, height = 5)
print(
  ggplot(d, aes(x = age, y = height, colour = Seed, group = Seed)) +
    geom_line() + geom_point() +
    labs(title = "Seed 301 与 303 的树高-年龄轨迹", x = "年龄", y = "树高", colour = "树") +
    theme_minimal()
)

# （2）配对差只有 6 个，正态性判断应同时结合差值图。
hist(wide$diff, main = "配对树高差的分布", xlab = "301 - 303", col = "skyblue")
print(shapiro.test(wide$diff))
cat("配对差均值：", mean(wide$diff), "\n")
if (!interactive()) dev.off()

# （3）双侧配对 t 检验。
paired_test <- t.test(wide$`301`, wide$`303`, paired = TRUE, alternative = "two.sided")
print(paired_test)

# （4）（5）同龄观测天然配对，独立样本 t 检验和方差比检验不适用。
cat("结果只描述编号 301 和 303 的两棵树，不能解释为家系或遗传差异。\n")
