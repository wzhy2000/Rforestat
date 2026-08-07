data <- read.csv("eg4_10.csv", stringsAsFactors = FALSE)
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

# 协方差检验
model.ancova <- aov(Height0 ~ DBH + plot, data = data)
summary(model.ancova)


###########   按协方差分析公式直接计算平方和（不调用 aov） ###################
x <- data$DBH
y <- data$Height0
group <- data$plot

N <- length(y)                 # 总样本量
J <- nlevels(group)            # 样地数
n_i <- as.numeric(table(group))
x_bar <- mean(x)
y_bar <- mean(y)
x_bar_i <- tapply(x, group, mean)
y_bar_i <- tapply(y, group, mean)

# 胸径和树高的总平方和、组内平方和与离差积和
SS_T_x <- sum((x - x_bar)^2)
SS_W_x <- sum((x - x_bar_i[group])^2)
SS_T_y <- sum((y - y_bar)^2)
SS_W_y <- sum((y - y_bar_i[group])^2)

SC_T_xy <- sum((x - x_bar) * (y - y_bar))
SC_W_xy <- sum((x - x_bar_i[group]) * (y - y_bar_i[group]))

# 表4.24的公式：调整后的平方和与胸径回归平方和
SS_R <- SC_T_xy^2 / SS_T_x
SS_T_y_adjusted <- SS_T_y - SC_T_xy^2 / SS_T_x
SS_W_y_adjusted <- SS_W_y - SC_W_xy^2 / SS_W_x
SS_B_y_adjusted <- SS_T_y_adjusted - SS_W_y_adjusted

# 表4.27的自由度、均方和 F 统计量
df_regression <- 1
df_between <- J - 1
df_within <- N - J - 1
df_total <- N - 1

MS_R <- SS_R / df_regression
MS_B_adjusted <- SS_B_y_adjusted / df_between
MS_W_adjusted <- SS_W_y_adjusted / df_within
F_regression <- MS_R / MS_W_adjusted
F_between_adjusted <- MS_B_adjusted / MS_W_adjusted

ancova_table <- data.frame(
  来源 = c("回归（胸径）", "调整后组间（样地）", "调整后组内", "总和"),
  平方和 = c(SS_R, SS_B_y_adjusted, SS_W_y_adjusted, SS_T_y),
  自由度 = c(df_regression, df_between, df_within, df_total),
  均方 = c(MS_R, MS_B_adjusted, MS_W_adjusted, NA),
  F值 = c(F_regression, F_between_adjusted, NA, NA),
  check.names = FALSE
)

ancova_table_display <- ancova_table
numeric_columns <- vapply(ancova_table_display, is.numeric, logical(1))
ancova_table_display[numeric_columns] <- lapply(
  ancova_table_display[numeric_columns], round, digits = 2
)
print(ancova_table_display, row.names = FALSE)
