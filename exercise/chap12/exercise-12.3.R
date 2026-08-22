# 习题 12.3：manyacidat 的识别检查与约化式 SUR

library(plantecophys)
library(systemfit)

data("manyacidat", package = "plantecophys")
needed <- c("Photo", "Ci", "Tleaf", "PARi", "treatment", "Curve")
d <- manyacidat[complete.cases(manyacidat[needed]), needed]
d$treatment <- factor(d$treatment)
d$Curve <- factor(d$Curve)

# （1）核对曲线和处理结构。
cat("完整观测数：", nrow(d), "；A-Ci 曲线数：", nlevels(d$Curve), "\n")
print(table(d$treatment))
print(summary(table(d$Curve)))

# （2）若 Photo 和 Ci 互为右侧变量、且两式使用相同外生变量，则没有方程特有的排除变量。
cat("互为结构式在现有相同外生变量下不满足排除限制，不能仅靠代数形式识别。\n")

# （3）无有效工具变量时，删除互为右侧变量，拟合约化式 SUR。
equations <- list(
  Photo = Photo ~ Tleaf + PARi + treatment,
  Ci = Ci ~ Tleaf + PARi + treatment
)
fit_sur <- systemfit(equations, method = "SUR", data = d)
print(summary(fit_sur))

# （4）逐方程报告拟合指标并检查残差。Curve 内相关尚未由 systemfit 处理。
model_metrics <- do.call(rbind, lapply(names(fit_sur$eq), function(name) {
  equation <- fit_sur$eq[[name]]
  observed <- equation$model[[1]]
  residual <- residuals(equation)
  data.frame(
    Equation = name,
    R2 = 1 - sum(residual^2) / sum((observed - mean(observed))^2),
    RMSE = sqrt(mean(residual^2)),
    MAE = mean(abs(residual))
  )
}))
print(model_metrics, row.names = FALSE)

if (!interactive()) grDevices::cairo_pdf(tempfile("exercise-12.3-", fileext = ".pdf"), width = 9, height = 7)
par(mfrow = c(2, 2))
for (name in names(fit_sur$eq)) {
  equation <- fit_sur$eq[[name]]
  observed <- equation$model[[1]]
  predicted <- fitted(equation)
  residual <- residuals(equation)
  plot(observed, predicted, main = paste("观测—预测：", name), xlab = "观测", ylab = "预测", pch = 16)
  abline(0, 1, col = "red", lty = 2)
  plot(predicted, residual, main = paste("残差：", name), xlab = "预测", ylab = "残差", pch = 16)
  abline(h = 0, col = "red", lty = 2)
}
if (!interactive()) dev.off()

photo_residual <- as.numeric(residuals(fit_sur$eq[[1]]))
curve_lag1 <- vapply(split(photo_residual, d$Curve), function(x) {
  if (length(x) < 3L) return(NA_real_)
  cor(x[-length(x)], x[-1])
}, numeric(1))
cat("Photo 方程曲线内残差 lag-1 相关中位数：", median(curve_lag1, na.rm = TRUE), "\n")
cat("当前 SUR 未给出 Curve 聚类稳健协方差；系数只能解释为条件关联，不能解释为因果效应。\n")
