# 习题 3.4：三个物种的出现频率和盖度等级分布

library(vegan)
library(dplyr)
library(tidyr)
library(ggplot2)
data("dune", package = "vegan")

# （1）Poatriv、Bracruta、Lolipere 分别对应 Poa trivialis、
# Brachythecium rutabulum、Lolium perenne；数值是盖度等级，不是个体计数。
species <- dune |> select(Poatriv, Bracruta, Lolipere)

# （2）统计离散盖度等级频数并绘制条形图。
frequency <- species |>
  pivot_longer(everything(), names_to = "species", values_to = "cover") |>
  count(species, cover, name = "frequency")
print(frequency)

if (!interactive()) grDevices::cairo_pdf(tempfile("exercise-3.4-", fileext = ".pdf"), width = 9, height = 5)
print(
  ggplot(frequency, aes(x = factor(cover), y = frequency)) +
    geom_col(fill = "forestgreen") +
    facet_wrap(~species) +
    labs(title = "三个物种的盖度等级频数", x = "盖度等级", y = "样方频数") +
    theme_minimal()
)
if (!interactive()) dev.off()

# （3）比较出现样方数、盖度位置和离散程度。
summary_table <- species |>
  summarise(across(
    everything(),
    list(
      present = ~sum(.x > 0),
      median = median,
      max = max,
      IQR = IQR
    )
  ))
print(summary_table)
