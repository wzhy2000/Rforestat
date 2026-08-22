# 习题 3.2：连续变量核查、对数变换和 IQR 标记

library(dplyr)
library(tidyr)
library(ggplot2)
data("picea", package = "forestat")

vars <- c("BRANCH", "LH", "LHCB", "LCW")
stopifnot(all(vars %in% names(picea)))
selected <- picea |> select(all_of(vars))

# （1）变量含义和单位应结合 help("picea", package="forestat") 核对。
audit <- selected |>
  summarise(across(
    everything(),
    list(
      n = ~sum(!is.na(.x)),
      nmiss = ~sum(is.na(.x)),
      min = ~min(.x, na.rm = TRUE),
      max = ~max(.x, na.rm = TRUE)
    )
  ))
print(audit)
strictly_positive <- vapply(selected, function(x) all(x[!is.na(x)] > 0), logical(1))
print(strictly_positive)

# （2）比较原尺度分布和潜在极端观测。
long_original <- selected |>
  pivot_longer(everything(), names_to = "variable", values_to = "value")

if (!interactive()) grDevices::cairo_pdf(tempfile("exercise-3.2-", fileext = ".pdf"), width = 9, height = 6)
print(
  ggplot(long_original, aes(x = variable, y = value)) +
    geom_boxplot(outlier.colour = "red") +
    geom_jitter(width = 0.12, alpha = 0.25) +
    facet_wrap(~variable, scales = "free") +
    labs(title = "四个连续变量的原尺度分布", x = NULL, y = "观测值") +
    theme_minimal()
)

# （3）仅对严格为正的变量取自然对数；IQR 规则只生成标记，不删记录。
positive_vars <- names(strictly_positive)[strictly_positive]
log_data <- selected |>
  mutate(across(all_of(positive_vars), log, .names = "log_{.col}")) |>
  select(starts_with("log_")) |>
  pivot_longer(everything(), names_to = "variable", values_to = "value")

if (nrow(log_data) > 0L) {
  print(
    ggplot(log_data, aes(x = variable, y = value)) +
      geom_boxplot(outlier.colour = "red") +
      geom_jitter(width = 0.12, alpha = 0.25) +
      facet_wrap(~variable, scales = "free") +
      labs(title = "适用变量的对数尺度分布", x = NULL, y = "log(观测值)") +
      theme_minimal()
  )
}
if (!interactive()) dev.off()

mark_iqr <- function(x) {
  result <- rep(FALSE, length(x))
  ok <- !is.na(x)
  q <- quantile(x[ok], c(0.25, 0.75), names = FALSE)
  limits <- q + c(-1.5, 1.5) * IQR(x[ok])
  result[ok] <- x[ok] < limits[1] | x[ok] > limits[2]
  result
}

flagged <- picea |>
  mutate(across(all_of(vars), mark_iqr, .names = "flag_{.col}"))
flag_vars <- paste0("flag_", vars)
flag_counts <- colSums(flagged[flag_vars], na.rm = TRUE)
print(flag_counts)
stopifnot(nrow(flagged) == nrow(picea))

# （4）比较完整数据与“仅用于敏感性分析的未标记子集”。
flagged$flag_any <- rowSums(flagged[flag_vars], na.rm = TRUE) > 0L
comparison <- bind_rows(
  selected |> summarise(across(everything(), list(mean = ~mean(.x, na.rm = TRUE), median = ~median(.x, na.rm = TRUE)))) |> mutate(dataset = "全部观测"),
  flagged |> filter(!flag_any) |> summarise(across(all_of(vars), list(mean = ~mean(.x, na.rm = TRUE), median = ~median(.x, na.rm = TRUE)))) |> mutate(dataset = "未被IQR标记")
)
print(comparison)
cat("IQR 标记不能单独作为删除依据；应回查原始记录、测量过程并做敏感性分析。\n")
