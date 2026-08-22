library(dplyr)
library(tidyr)
tree_wide <- read.csv("../../data/example-4.13.csv", stringsAsFactors = FALSE)
tree.long <- tree_wide %>%
  pivot_longer(
    cols = c(Height0, Height.after.3.years, Height.after.6.years),
    names_to = "time",
    values_to = "Height"
  ) %>%
  mutate(
    plot = factor(plot),
    sampleid = factor(sampleid),
    time = factor(time)
  )

model <- aov(Height ~ plot * time + Error(sampleid/(time)), data = tree.long)
summary(model)

