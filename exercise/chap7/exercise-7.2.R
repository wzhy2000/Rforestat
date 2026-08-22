# 习题 7.2：按样地汇总存活比例并拟合准二项模型

library(dplyr)
data("larch", package = "forestat")
stopifnot(all(c("LIFE", "PLOT", "SD") %in% names(larch)))

# （1）先核对 LIFE 编码，绝不根据变量名猜测“1=存活”。
life_table <- table(larch$LIFE, useNA = "ifany")
print(life_table)

outcome_available <- FALSE
if (is.numeric(larch$LIFE) && all(na.omit(unique(larch$LIFE)) %in% c(0, 1))) {
  larch$survived <- ifelse(is.na(larch$LIFE), NA, larch$LIFE == 1)
  outcome_available <- TRUE
} else if (all(c("alive", "dead") %in% na.omit(unique(tolower(larch$LIFE))))) {
  larch$survived <- ifelse(is.na(larch$LIFE), NA, tolower(larch$LIFE) == "alive")
  outcome_available <- TRUE
}

if (!outcome_available) {
  cat(
    "当前 forestat::larch 的 LIFE 是健康状态字符变量，且没有明确 death 类别；\n",
    "无法从该版本构造有科学依据且有两类取值的存活响应，因此不强行拟合准二项模型。\n",
    "出版前应补充明确的数据字典或更换含死亡结局的同步数据。\n",
    sep = ""
  )
} else {
  # （2）以样地为二项分母统计单位。
  d <- larch |>
    group_by(PLOT) |>
    summarise(
      alive = sum(survived, na.rm = TRUE),
      total = sum(!is.na(survived)),
      SD_mean = mean(SD, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(observed_proportion = alive / total)
  print(d)
  stopifnot(all(d$alive >= 0L), all(d$alive <= d$total), all(d$total > 0L))

  if (length(unique(d$alive / d$total)) < 2L) {
    cat("汇总后的存活比例没有变异，无法估计 SD_mean 效应。\n")
  } else {
    # （3）拟合准二项模型并报告离散参数。
    model <- glm(cbind(alive, total - alive) ~ SD_mean, family = quasibinomial, data = d)
    print(summary(model))
    print(confint.default(model))
    cat("离散参数：", summary(model)$dispersion, "\n")

    # （4）绘制观测比例和预测曲线。
    grid <- data.frame(SD_mean = seq(min(d$SD_mean), max(d$SD_mean), length.out = 200))
    grid$fit <- predict(model, newdata = grid, type = "response")
    if (!interactive()) grDevices::cairo_pdf(tempfile("exercise-7.2-", fileext = ".pdf"), width = 7, height = 5)
    plot(d$SD_mean, d$observed_proportion, cex = 0.5 + 2 * sqrt(d$total / max(d$total)), pch = 16, xlab = "平均林分密度", ylab = "观测存活比例")
    lines(grid$SD_mean, grid$fit, col = "red", lwd = 2)
    if (!interactive()) dev.off()
    cat("曲线只描述林分密度与存活比例的统计关联。\n")
  }
}
