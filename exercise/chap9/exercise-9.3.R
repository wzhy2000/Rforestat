# 习题 9.3：Loblolly 总体平滑与树木随机效应

library(mgcv)
library(ggplot2)
data("Loblolly", package = "datasets")

# （1）核对每棵树的观测时点，绘制原始轨迹和总体 loess。
print(table(Loblolly$Seed))
stopifnot(all(table(Loblolly$Seed) == 6L), length(unique(Loblolly$age)) == 6L)
if (!interactive()) grDevices::cairo_pdf(tempfile("exercise-9.3-", fileext = ".pdf"), width = 9, height = 6)
print(
  ggplot(Loblolly, aes(age, height, group = Seed, colour = Seed)) +
    geom_line(alpha = 0.55) + geom_point() +
    geom_smooth(aes(group = 1), method = "loess", colour = "black", se = TRUE, linewidth = 1.2) +
    labs(title = "14 棵树的生长轨迹与总体 loess", x = "年龄", y = "树高") +
    theme_minimal()
)

# （2）低复杂度总体平滑 + Seed 随机效应，不逐树拟合高自由度平滑。
model <- gam(height ~ s(age, k = 4) + s(Seed, bs = "re"), data = Loblolly, method = "REML")
print(summary(model))

# （3）检查 edf、k-index、残差和稳定性。
gam.check(model)
plot(model, residuals = TRUE, pages = 1)
if (!interactive()) dev.off()
cat("若 k-index 提示维度不足，可谨慎增加 k；不能仅为贴合样本而无限增大。\n")

# （4）Seed 随机效应仅描述树木间偏离。
random_term <- grep("s\\(Seed\\)", names(coef(model)), value = TRUE)
print(coef(model)[random_term])
cat("Seed 是树木标识，不支持家系、育种或遗传推断。\n")
