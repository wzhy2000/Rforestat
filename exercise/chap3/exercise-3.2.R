library(dplyr)
library(tidyr)
library(ggplot2)
library(forestat)
data(picea)

selected.data <- picea %>% select(BRANCH, LH, LHCB, LCW)
long_data <- selected.data %>%
  pivot_longer(cols = everything(), names_to = "Parameter", values_to = "Value") %>%
  mutate(Value = log10(Value + 1))
ggplot(long_data, aes(x = Parameter, y = Value, fill = Parameter)) +
  geom_boxplot(outlier.color = "red") +
  labs(x = "变量", y = "对数转换值") +
  theme_minimal()
removeOutliers <- function(data, column) {
  Q1 <- quantile(data[[column]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data[[column]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR
  upper <- Q3 + 1.5 * IQR
  data[data[[column]] >= lower & data[[column]] <= upper, ]
}
cleaned_data <- selected.data
for (col in colnames(cleaned_data)) {
  cleaned_data <- removeOutliers(cleaned_data, col)
}
