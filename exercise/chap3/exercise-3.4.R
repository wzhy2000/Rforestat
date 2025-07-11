library(vegan)
library(dplyr)
library(tidyr)
library(ggplot2)
data(dune)
selected_species <- c("Poatriv", "Bracruta", "Lolipere")
dune %>%
  select(all_of(selected_species))
dune_long <- dune %>%
  select(all_of(selected_species)) %>%
  pivot_longer(cols = everything(), names_to = "Species", values_to = "Abundance")
ggplot(dune_long, aes(x = Abundance, fill = Species)) +
  geom_histogram(binwidth = 1, color = "black", show.legend = FALSE) +
  facet_wrap(~ Species, scales = "free_y") +
  theme_minimal() +
  labs(title = "3个物种的丰度分布直方图",
       x = "丰度", y = "样方数量") +
  scale_fill_brewer(palette = "Set2")
