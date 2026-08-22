# 习题 8.2：龄组与样地随机结构比较

library(lme4)
library(performance)
data("larch", package = "forestat")
d <- subset(larch, complete.cases(AGE.GROUP, PLOT, H, CW, D))
d$AGE.GROUP <- factor(d$AGE.GROUP)
d$PLOT <- factor(d$PLOT)

# （1）用交叉表判断 PLOT 是否只属于一个 AGE.GROUP。
design_table <- with(d, table(AGE.GROUP, PLOT))
print(design_table > 0)
age_groups_per_plot <- colSums(design_table > 0)
nested <- all(age_groups_per_plot == 1L)
cat("PLOT 是否严格嵌套于 AGE.GROUP：", nested, "\n")
if (!nested) {
  cat("跨龄组的 PLOT：", paste(names(age_groups_per_plot)[age_groups_per_plot > 1L], collapse = ", "), "\n")
}

# （2）固定效应相同；按真实结构拟合嵌套或交叉两层模型。
single_level <- lmer(H ~ CW + D + (1 | PLOT), data = d, REML = FALSE)
if (nested) {
  two_level <- lmer(H ~ CW + D + (1 | AGE.GROUP / PLOT), data = d, REML = FALSE)
  structure_label <- "PLOT 嵌套于 AGE.GROUP"
} else {
  two_level <- lmer(H ~ CW + D + (1 | AGE.GROUP) + (1 | PLOT), data = d, REML = FALSE)
  structure_label <- "AGE.GROUP 与 PLOT 交叉随机截距"
}
cat("两层模型结构：", structure_label, "\n")

# （3）在同一数据和 ML 估计下比较信息准则、边际/条件 R²及近似 LRT。
print(AIC(single_level, two_level))
print(BIC(single_level, two_level))
print(anova(single_level, two_level))
print(r2_nakagawa(single_level))
print(r2_nakagawa(two_level))
cat("新增方差分量的零假设位于边界，普通 LRT 的卡方近似需谨慎。\n")

# （4）检查奇异性、方差边界和残差。
print(VarCorr(two_level))
cat("两层模型是否奇异：", isSingular(two_level), "\n")
if (!interactive()) grDevices::cairo_pdf(tempfile("exercise-8.2-", fileext = ".pdf"), width = 8, height = 4)
par(mfrow = c(1, 2))
plot(fitted(two_level), resid(two_level), pch = 16, cex = 0.35, col = rgb(0, 0, 0, 0.2), xlab = "拟合值", ylab = "残差")
qqnorm(resid(two_level)); qqline(resid(two_level), col = "red")
if (!interactive()) dev.off()
cat("最终随机结构必须服从采样设计；奇异或边界估计时应优先简化。\n")
