data <- read.csv("../../data/eg4_10.csv", stringsAsFactors = FALSE)
data$plot <- factor(data$plot,
                    levels = c("p1", "p2", "p3", "p4"))

# 检查变量
str(data)
table(data$plot)

# 不含交互项的模型
model.add <- lm(Height0 ~ DBH + plot, data = data)

# 包含交互项的模型
model.int <- lm(Height0 ~ DBH * plot, data = data)

# 检验 DBH:plot 交互作用
anova(model.add, model.int)


model.ancova <- aov(
  Height0 ~ DBH + plot,
  data = data
)

summary(model.ancova)
