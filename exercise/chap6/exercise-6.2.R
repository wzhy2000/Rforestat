# 习题 6.2：CO2 数据的多重线性回归

data("CO2", package = "datasets")

# （1）核对变量类型、层级和重复观测结构。
print(str(CO2[c("conc", "Treatment", "Type", "Plant")]))
print(with(CO2, table(Type, Plant)))
cat("每株植物在 7 个浓度点重复观测；Plant 嵌套于 Type。\n")
stopifnot(all(table(CO2$Plant) == 7L))

# （2）拟合预先规定的主效应加性模型，不同时加入嵌套 Plant 固定效应。
model <- lm(uptake ~ conc + Treatment + Type, data = CO2)

# （3）报告系数、区间、R²和调整 R²。
model_summary <- summary(model)
print(model_summary)
print(confint(model))
cat("R²：", model_summary$r.squared, "；调整 R²：", model_summary$adj.r.squared, "\n")
cat("分类变量系数均相对于输出中显示的基准水平解释。\n")

# （4）诊断普通 lm；它忽略 Plant 内相关，只作教学示例。
if (!interactive()) grDevices::cairo_pdf(tempfile("exercise-6.2-", fileext = ".pdf"), width = 9, height = 6)
par(mfrow = c(1, 2))
plot(model, which = 1)
plot(model, which = 2)
if (!interactive()) dev.off()
cat("普通 lm 忽略同一 Plant 内相关；正式推断应采用混合效应或适当相关结构。\n")
