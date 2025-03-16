library(tidyr)
data <- read.csv("data-{eg4-10}.csv")
tree.long <- data %>%
  pivot_longer(
    cols = c(Height0, Height.after.3.years, Height.after.6.years),
    names_to = "time",
    values_to = "Height"
  )
tree.long$plot <- as.factor(tree.long$plot)
tree.long$sampleid <- as.factor(tree.long$sampleid)
tree.long$time <- as.factor(tree.long$time)
model <- aov(Height ~ plot * time + Error(sampleid/(plot * time)), data = tree.long)
summary(model)